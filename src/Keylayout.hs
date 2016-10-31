{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}

module Keylayout
    ( printKeylayout
    , toKeylayout
    ) where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((⊕))
import Data.List.Unicode ((∖))
import Util

import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Text.Lazy as L (pack)
import qualified Data.Text.Lazy.Encoding as L (encodeUtf8)
import Lens.Micro.Platform (view, over, _1, _2)
import Text.XML.Light

import Layout.Key (filterKeyOnShiftstatesM, addCapslock, letterToDeadKey)
import Layout.Layout (addDefaultKeys, addDefaultKeysWith, unifyShiftstates)
import Layout.Types
import Lookup.MacOS (modifierAndString, posAndCode, actionAndChar)
import PresetDeadKey (presetDeadKeyToDeadKey)
import PresetLayout (defaultKeys, defaultMacKeys)

prepareLayout ∷ Layout → Logger Layout
prepareLayout =
    addDefaultKeysWith const defaultMacKeys >>>
    addDefaultKeys defaultKeys >>>
    (_keys ∘ traverse)
        (filterKeyOnShiftstatesM supportedShiftstate >$>
        addCapslock)

supportedShiftstate ∷ Shiftstate → Logger Bool
supportedShiftstate = fmap and ∘ traverse supportedModifier ∘ toList

supportedModifier ∷ Modifier → Logger Bool
supportedModifier modifier
  | modifier ∈ map fst modifierAndString = pure True
  | otherwise = False <$ tell [show' modifier ⊕ " is not supported in keylayout"]

attr ∷ String → String → Attr
attr = Attr ∘ unqual

printKeylayout ∷ Element → BL.ByteString
printKeylayout = L.encodeUtf8 ∘ L.pack ∘ (header ⊕) ∘ (⊕ "\n") ∘ ppElement
  where
    header = unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<!DOCTYPE keyboard SYSTEM \"file://localhost/System/Library/DTDs/KeyboardLayout.dtd\">"
        ]

toKeylayout ∷ Layout → Logger Element
toKeylayout = prepareLayout >=> toKeylayout'

toKeylayout' ∷ Layout → Logger Element
toKeylayout' layout = removeEmptyElementsInside ∘ unode "keyboard" ∘ (,)
    [ attr "group" "126"
    , attr "id" "-1337"
    , attr "name" (view (_info ∘ _fullName) layout)
    ] <$> sequence
    [ pure $ unode "layouts" layoutElement
    , pure $ toModifierMap shiftstates
    , keyMapSetElementOutputToActions deadKeys <$> toKeyMapSet keys
    , pure $ unode "actions" (map deadKeyToAction deadKeys ⧺ deadKeysToActions deadKeys)
    , pure $ unode "terminators" (mapMaybe deadKeyToTerminator deadKeys)
    ]
  where
    (keys, shiftstates) = unifyShiftstates (view _keys layout)
    deadKeys = nub (concatMap (mapMaybe letterToDeadKey ∘ view _letters) keys)
    deadKeysToActions =
        concatMap deadKeyToActions >>>
        groupWith' fst >>>
        over (traverse ∘ _2 ∘ traverse) snd >>>
        map (\(s, elms) → (s, emptyAction s : elms)) >>>
        map (unode "action" ∘ over _1 ((:[]) ∘ attr "id"))
    emptyAction s = unode "when" [attr "state" "none", attr "output" s]
    layoutElement = unode "layout"
        [ attr "first" "0"
        , attr "last" "0"
        , attr "modifiers" "defaultModifierMap"
        , attr "mapSet" "defaultKeyMapSet"
        ]

removeEmptyElementsInside ∷ Element → Element
removeEmptyElementsInside e = e { elContent = mapMaybe removeEmptyElements' (elContent e) }

removeEmptyElements ∷ Element → Maybe Element
removeEmptyElements e
  | null (elAttribs e) ∧ null (elContent e) ∧ null (elLine e) = Nothing
  | otherwise = Just e

removeEmptyElements' ∷ Content → Maybe Content
removeEmptyElements' (Elem e) = Elem ∘ removeEmptyElementsInside <$> removeEmptyElements e
removeEmptyElements' c = Just c

toModifierMap ∷ [Shiftstate] → Element
toModifierMap states = unode "modifierMap" ∘ (,)
    [ attr "id" "defaultModifierMap"
    , attr "defaultIndex" "0"
    ] ∘
    zipWith toKeyMapSelect [0..] $ map (toModifiers (ignored states)) states
  where
    ignored =
        (map fst modifierAndString ∖) ∘ concatMap toList >>>
        mapMaybe (`lookup` modifierAndString) >>>
        map (⊕ "?")

toKeyMapSelect ∷ Int → [Element] → Element
toKeyMapSelect i = unode "keyMapSelect" ∘ (,) [attr "mapIndex" (show i)]

toModifiers ∷ [String] → Shiftstate → [Element]
toModifiers ignored state =
    (:[]) ∘ unode "modifier" ∘ (:[]) ∘ attr "keys" ∘ unwords $ modifiers ⧺ ignored
  where
    modifiers = map modifierToString (toList state)

modifierToString ∷ Modifier → String
modifierToString modifier = fromMaybe e (lookup modifier modifierAndString)
  where e = error $ show' modifier ⊕ " is not supported in keylayout"

toKeyMapSet ∷ [Key] → Logger Element
toKeyMapSet =
    transpose ∘ map (\key → (,) (view _pos key) <$> view _letters key) >>>
    zipWithM toKeyMap [0..] >$>
    unode "keyMapSet" ∘ (,) [attr "id" "defaultKeyMapSet"]

toKeyMap ∷ Int → [(Pos, Letter)] → Logger Element
toKeyMap i =
    traverse (uncurry printKey) >$>
    catMaybes >>>
    unode "keyMap" ∘ (,) [attr "index" (show i)]

printKey ∷ Pos → Letter → Logger (Maybe Element)
printKey pos letter = runMaybeT $
    unode "key" <$> sequence
      [ attr "code" <$> printPos pos
      , printLetter letter
      ]

printPos ∷ Pos → MaybeT Logger String
printPos pos = maybe e (pure ∘ show) (lookup pos posAndCode)
  where e = tellMaybeT [show' pos ⊕ " is not supported in keylayout"]

printLetter ∷ Letter → MaybeT Logger Attr
printLetter (Char c) = pure (attr "output" [c])
printLetter (Ligature _ s) = pure (attr "output" s)
printLetter (Dead dead) = printLetter (CustomDead Nothing (presetDeadKeyToDeadKey dead))
printLetter (CustomDead _ (DeadKey name _ _)) = pure (attr "action" ("dead:" ⊕ name))
printLetter (Action a) | Just c ← lookup a actionAndChar = pure (attr "output" [c])
printLetter LNothing = MaybeT (pure Nothing)
printLetter letter = tellMaybeT [show' letter ⊕ " is not supported in keylayout"]

deadKeyToAction ∷ DeadKey → Element
deadKeyToAction (DeadKey name _ _) =
    unode "action" ([attr "id" name'], unode "when" [attr "state" "none", attr "next" name'])
  where name' = "dead:" ⊕ name

deadKeyToActions ∷ DeadKey → [(String, Element)]
deadKeyToActions (DeadKey name _ stringMap) =
    map (prependLig *** toAttr) stringMap
  where
    toAttr outString = unode "when" [attr "state" name', attr "output" outString]
    name' = "dead:" ⊕ name

deadKeyToTerminator ∷ DeadKey → Maybe Element
deadKeyToTerminator (DeadKey name (Just baseChar) _) = pure $
    unode "when" [attr "state" ("dead:" ⊕ name), attr "output" [baseChar]]
deadKeyToTerminator _ = Nothing

keyMapSetElementOutputToActions ∷ [DeadKey] → Element → Element
keyMapSetElementOutputToActions =
    overElements ∘ overElements ∘ keyElementOutputToAction
  where
    overElements f elm = elm { elContent = map (overContent f) (elContent elm) }
    overContent f (Elem e) = Elem (f e)
    overContent _ x = x

keyElementOutputToAction ∷ [DeadKey] → Element → Element
keyElementOutputToAction deads elm = elm { elAttribs = map outputToAction (elAttribs elm) }
  where
    outputToAction a
      | attrKey a ≡ unqual "output"
      ∧ attrVal a ∈ concatMap (map fst ∘ __stringMap) deads
      = a { attrKey = unqual "action", attrVal = prependLig (attrVal a) }
      | otherwise = a

prependLig ∷ String → String
prependLig "" = ""
prependLig [x] = [x]
prependLig s = "lig:" ⊕ s

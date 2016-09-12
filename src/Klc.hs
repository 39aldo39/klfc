{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Klc
    ( printKlcData
    , toKlcData
    ) where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((⊕))
import Util (show', toString, ifNonEmpty, (>$>), concatMapM, tellMaybeT, sequenceTuple)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Writer (runWriter, tell)
import Lens.Micro.Platform (view, over)

import Layout.Key (letterToLigatureString, filterKeyOnShiftstates)
import Layout.Layout (setNullChars, unifyShiftstates)
import Layout.Types
import Lookup.Linux (posAndScancode)
import Lookup.Windows
import PresetDeadKey (presetDeadKeyToDeadKey)

prepareLayout ∷ Layout → Logger Layout
prepareLayout =
    _singletonKeys
        emptySingletonKeys >$>
    over _keys
        (over (traverse ∘ _shiftstates ∘ traverse) altGrToControlAlt >>>
        over (traverse ∘ _letters ∘ traverse) deadToCustomDead >>>
        setNullChars)

emptySingletonKeys ∷ [SingletonKey] → Logger [SingletonKey]
emptySingletonKeys [] = pure []
emptySingletonKeys xs = xs <$ tell ["singleton keys are not supported in KLC"]

deadToCustomDead ∷ Letter → Letter
deadToCustomDead (Dead d) = CustomDead Nothing (presetDeadKeyToDeadKey d)
deadToCustomDead l = l

supportedShiftstate ∷ Shiftstate → Logger Bool
supportedShiftstate = fmap and ∘ traverse supportedModifier ∘ toList

supportedModifier ∷ Modifier → Logger Bool
supportedModifier modifier
  | modifier ∈ map fst modifierAndWinShiftstate = pure True
  | otherwise = False <$ tell [show' modifier ⊕ " is not supported in KLC"]


-- KLC DATA

data KlcKey = KlcKey
    { __klcPos ∷ String
    , __klcShortcutPos ∷ String
    , __klcCapslock ∷ Bool
    , __klcLetters ∷ [String]
    , __klcComment ∷ String
    } deriving (Show, Read)
printKlcKey ∷ KlcKey → String
printKlcKey (KlcKey pos shortcutPos caps letters comment) = intercalate "\t" $
    [ pos
    , shortcutPos
    , show (fromEnum caps)
    ] ⧺ dropWhileEnd (≡ "-1") letters
    ⧺ [ifNonEmpty ("// " ⊕) comment]

printKlcKeys ∷ [KlcKey] → [String]
printKlcKeys = map printKlcKey

data KlcLigature = KlcLigature
    { __ligPos ∷ String
    , __ligShiftstate ∷ Int
    , __ligString ∷ String
    } deriving (Show, Read)
printKlcLigature ∷ KlcLigature → String
printKlcLigature (KlcLigature pos shiftstate s) = intercalate "\t" $
    [ pos
    , show shiftstate
    ] ⧺ map (printf "%04x") s
    ⧺ [ifNonEmpty ("// " ⊕) s]

printKlcLigatures ∷ [KlcLigature] → [String]
printKlcLigatures [] = []
printKlcLigatures xs = "" : "LIGATURE" : "" : map printKlcLigature xs

data KlcDeadKey = KlcDeadKey
    { __klcDeadName ∷ String
    , __klcBaseChar ∷ Char
    , __klcCharMap ∷ [(Char, Char)]
    } deriving (Show, Read)
printKlcDeadKey ∷ KlcDeadKey → [String]
printKlcDeadKey (KlcDeadKey name baseChar charMap) =
    "" :
    ("DEADKEY " ⊕ printf "%04x" baseChar ⊕ "\t// " ⊕ name) :
    "" :
    map (uncurry showPair) charMap
  where
    showPair k v = intercalate "\t" [printf "%04x" k, printf "%04x" v, "// " ⊕ [k] ⊕ " → " ⊕ [v] ∷ String]

printKlcDeadKeys ∷ [KlcDeadKey] → [String]
printKlcDeadKeys = concatMap printKlcDeadKey

data KlcData = KlcData
    { __klcInformation ∷ Information
    , __klcShiftstates ∷ [WinShiftstate]
    , __klcKeys ∷ [KlcKey]
    , __klcLigatures ∷ [KlcLigature]
    , __klcDeadKeys ∷ [KlcDeadKey]
    } deriving (Show, Read)
printKlcData ∷ KlcData → String
printKlcData (KlcData info winShiftstates keys ligatures deadKeys) = unlines $
    [ "KBD\t" ⊕ view _name info ⊕ "\t" ⊕ show (view _fullName info) ] ⧺
    [ "\nCOPYRIGHT\t" ⊕ show copyright | copyright ← maybeToList $ view _copyright info ] ⧺
    [ "\nCOMPANY\t"   ⊕ show company   | company   ← maybeToList $ view _company   info ] ⧺
    [ "\nLOCALEID\t"  ⊕ show localeId  | localeId  ← maybeToList $ view _localeId  info ] ⧺
    [ "\nVERSION\t"   ⊕ version        | version   ← maybeToList $ view _version   info ] ⧺
    [ ""
    , "SHIFTSTATE"
    , ""
    ] ⧺ map show winShiftstates ⧺
    [ ""
    , "LAYOUT"
    , ""
    ] ⧺ printKlcKeys keys
    ⧺ printKlcLigatures ligatures
    ⧺ printKlcDeadKeys deadKeys ⧺
    [ ""
    , ""
    , "ENDKBD"
    ]


-- TO KLC DATA

toKlcData ∷ Layout → Logger KlcData
toKlcData =
    prepareLayout >=>
    (_keys ∘ traverse) (filterKeyOnShiftstates supportedShiftstate) >=>
    toKlcData'

toKlcData' ∷ Layout → Logger KlcData
toKlcData' layout =
    KlcData
      <$> pure (view _info layout)
      <*> pure (map winShiftstateFromShiftstate states)
      <*> (catMaybes <$> traverse toKlcKey keys)
      <*> toKlcLigatures layout
      <*> toKlcDeadKeys keys
  where
    (keys, states) = unifyShiftstates (view _keys layout)

toKlcKey ∷ Key → Logger (Maybe KlcKey)
toKlcKey key = runMaybeT $
    KlcKey
      <$> printPos (view _pos key)
      <*> printShortcutPos (view _shortcutPos key)
      <*> pure (view _capslock key)
      <*> lift (traverse printLetter (view _letters key))
      <*> pure comment
  where
    comment = "QWERTY " ⊕ toString (view _pos key) ⊕ ifNonEmpty (": " ⊕) (intercalate ", " letterComments)
    letterComments = map toString (dropWhileEnd unsupported (view _letters key))
    unsupported = (≡) "-1" ∘ fst ∘ runWriter ∘ printLetter

printPos ∷ Pos → MaybeT Logger String
printPos pos = maybe e pure $ printf "%02x" <$> lookup pos posAndScancode
  where e = tellMaybeT [show' pos ⊕ " is not supported in KLC"]

printShortcutPos ∷ Pos → MaybeT Logger String
printShortcutPos pos = maybe e pure $ lookup pos posAndString
  where e = tellMaybeT [show' pos ⊕ " is not supported in KLC"]

printLetter ∷ Letter → Logger String
printLetter (Char c)
    | isAscii c ∧ isAlphaNum c = pure [c]
    | otherwise = pure (printf "%04x" c)
printLetter (Ligature _ _) = pure "%%"
printLetter (Dead d) = printLetter (CustomDead Nothing (presetDeadKeyToDeadKey d))
printLetter (CustomDead _ (DeadKey _ (Just c) _)) = (⧺"@") <$> printLetter (Char c)
printLetter l@(CustomDead _ (DeadKey _ Nothing _ )) = "-1" <$ tell [show' l ⊕ " has no base character"]
printLetter LNothing = pure "-1"
printLetter l = "-1" <$ tell [show' l ⊕ " is not supported in KLC"]

toKlcLigatures ∷ Layout → Logger [KlcLigature]
toKlcLigatures = concatMapM toKlcLigature ∘ view _keys

toKlcLigature ∷ Key → Logger [KlcLigature]
toKlcLigature key = fmap catMaybes ∘ sequence $ do
    (shiftstate, letter) ← view _shiftstates key `zip` view _letters key
    maybeToList $ toLigature pos (winShiftstateFromShiftstate shiftstate) <$> letterToLigatureString letter
  where
    pos = view _pos key

toLigature ∷ Pos → Int → String → Logger (Maybe KlcLigature)
toLigature pos shiftState s = runMaybeT $
    KlcLigature
      <$> printShortcutPos pos
      <*> pure shiftState
      <*> pure s

toKlcDeadKeys ∷ [Key] → Logger [KlcDeadKey]
toKlcDeadKeys = concatMapM toKlcDeadKey

toKlcDeadKey ∷ Key → Logger [KlcDeadKey]
toKlcDeadKey = fmap catMaybes ∘ traverse toKlcDeadKey' ∘ view _letters

toKlcDeadKey' ∷ Letter → Logger (Maybe KlcDeadKey)
toKlcDeadKey' (Dead d) = toKlcDeadKey' (CustomDead Nothing (presetDeadKeyToDeadKey d))
toKlcDeadKey' (CustomDead _ (DeadKey dName (Just c) stringMap)) = Just ∘ KlcDeadKey dName c <$> charMap
  where
    charMap = traverse (sequenceTuple ∘ (char *** char)) stringMap
    char ∷ String → Logger Char
    char [x]      = pure x
    char []       = '\0' <$ tell ["empty string in dead key ‘" ⊕ dName ⊕ "’ in KLC"]
    char xs@(x:_) = x <$ tell ["the string ‘" ⊕ xs ⊕ "’ is shortened to ‘" ⊕ [x] ⊕ "’ in dead key ‘" ⊕ dName ⊕ "’ in KLC"]
toKlcDeadKey' _ = pure Nothing

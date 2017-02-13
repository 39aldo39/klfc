{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}

module Pkl
    ( printPklData
    , toPklData
    , printLayoutData
    , toLayoutData
    ) where

import BasePrelude
import Prelude.Unicode hiding ((∈), (∉))
import Data.Monoid.Unicode ((⊕))
import Data.Foldable.Unicode ((∈), (∉))
import Util (toString, show', ifNonEmpty, (>$>), nubOn, filterOnSnd, filterOnSndM, sequenceTuple, tellMaybeT)
import qualified WithPlus as WP (fromList, singleton)

import Control.Monad.State (evalState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer (tell)
import Lens.Micro.Platform (view, over, (<&>))

import Layout.Key (letterToDeadKey, filterKeyOnShiftstatesM, toIndexedCustomDeadKey)
import Layout.Layout (addDefaultKeysWith, getDefaultKeys, setNullChars, unifyShiftstates, getLetterByPosAndShiftstate)
import qualified Layout.Modifier as M
import Layout.ModifierEffect (defaultModifierEffect)
import qualified Layout.Pos as P
import Layout.Types
import Lookup.Linux (posAndScancode)
import Lookup.Windows
import PresetLayout (defaultKeys, defaultFullLayout)

data IsExtend = Extend | NotExtend

prepareLayout ∷ Layout → Layout
prepareLayout =
    addDefaultKeysWith getDefaultKeys' defaultKeys >>>
    over _singletonKeys (filter (not ∘ isAltRToAltGr)) >>>
    over _keys
        (over (traverse ∘ _shiftstates ∘ traverse) altGrToControlAlt >>>
        flip evalState 1 ∘ (traverse ∘ _letters ∘ traverse) toIndexedCustomDeadKey >>>
        setNullChars)
  where
    getDefaultKeys' keys = getDefaultKeys keys ∘ filterNonExtend
    filterNonExtend = over _keys (filter (any (M.Extend ∉) ∘ view _shiftstates))

supportedShiftstate ∷ IsExtend → Shiftstate → Logger Bool
supportedShiftstate NotExtend shiftstate = and <$> traverse supportedModifier (toList shiftstate)
supportedShiftstate Extend shiftstate
  | M.Extend ∉ shiftstate = pure False
  | shiftstate ≡ WP.singleton M.Extend = pure True
  | otherwise = False <$ tell ["extend mappings with other modifiers is not supported in PKL"]

supportedModifier ∷ Modifier → Logger Bool
supportedModifier M.Extend = pure False
supportedModifier modifier
  | modifier ∈ map fst modifierAndWinShiftstate = pure True
  | otherwise = False <$ tell [show' modifier ⊕ " is not supported in PKL"]


-- PKL DATA

data PklKey
    = PklKey
        { __pklPos ∷ String
        , __pklShortcutPos ∷ String
        , __pklCapslock ∷ Bool
        , __pklLetters ∷ [String]
        , __pklComment ∷ String
        }
    | PklSpecialKey
        { __pklPos ∷ String
        , __pklSpecialString ∷ String
        , __pklComment ∷ String
        }
    deriving (Show, Read)

printPklKey ∷ PklKey → String
printPklKey (PklKey pos shortcutPos caps letters comment) = intercalate "\t" $
    [ pos ⊕ " = " ⊕ shortcutPos
    , show (fromEnum caps)
    ] ⧺ dropWhileEnd (≡ "--") letters ⧺
    [ "; " ⊕ comment | not (null comment) ]
printPklKey (PklSpecialKey pos string comment) = intercalate "\t" $
    [ pos ⊕ " = " ⊕ string ] ⧺
    [ "; " ⊕ comment | not (null comment) ]

pklKeyToSingleString ∷ PklKey → Maybe String
pklKeyToSingleString PklKey{} = Nothing
pklKeyToSingleString (PklSpecialKey _ s _) = Just s

data PklDeadKey = PklDeadKey
    { __pklName ∷ String
    , __pklInt ∷ Int
    , __pklCharMap ∷ [(Char, Char)]
    } deriving (Show, Read)
printPklDeadKey ∷ PklDeadKey → [String]
printPklDeadKey (PklDeadKey name int charMap) =
    "" :
    ("[deadkey" ⊕ show int ⊕ "]\t; " ⊕ name) :
    map (uncurry showPair) charMap
  where
    showPair k v = intercalate "\t"
        [ show (ord k)
        , "="
        , show (ord v)
        , "; " ⊕ [k] ⊕ " → " ⊕ [v]
        ]

printPklKeys ∷ [PklKey] → [String]
printPklKeys = map printPklKey ∘ filter ((≢) (Just "--") ∘ pklKeyToSingleString)

printPklDeadKeys ∷ [PklDeadKey] → [String]
printPklDeadKeys [] = []
printPklDeadKeys xs = ("":) (concatMap printPklDeadKey xs)

data LayoutData = LayoutData
    { __pklInformation ∷ Information
    , __pklHomepage ∷ String
    , __pklExtendPos ∷ Maybe String
    , __pklShiftstates ∷ [WinShiftstate]
    , __pklKeys ∷ [PklKey]
    , __pklDeadKeys ∷ [PklDeadKey]
    } deriving (Show, Read)
data PklData = PklData
    { __pklDataName ∷ String
    , __pklExtendKeys ∷ [PklKey]
    } deriving (Show, Read)

printLayoutData ∷ String → LayoutData → String
printLayoutData date (LayoutData info homepage maybeExtendPos shiftstates keys deadKeys) = unlines $
    [ "[informations]"
    , "layoutname   = " ⊕ view _fullName info
    , "layoutcode   = " ⊕ view _name info ] ⧺
    [ "localeid     = " ⊕ localeId  | localeId  ← maybeToList $ view _localeId  info ] ⧺
    [ "" ] ⧺
    [ "copyright    = " ⊕ copyright | copyright ← maybeToList $ view _copyright info ] ⧺
    [ "company      = " ⊕ company   | company   ← maybeToList $ view _company   info ] ⧺
    [ "homepage     = " ⊕ homepage ] ⧺
    [ "version      = " ⊕ version   | version   ← maybeToList $ view _version   info ] ⧺
    [ ""
    , "generated_at = " ⊕ date
    , ""
    , ""
    , "[global]" ] ⧺
    [ "extend_key = " ⊕ pos | pos ← maybeToList maybeExtendPos ] ⧺
    [ "shiftstates = " ⊕ intercalate ":" (map show shiftstates)
    , ""
    , ""
    , "[layout]"
    ] ⧺ printPklKeys keys
    ⧺ printPklDeadKeys deadKeys

type IsCompact = Bool
printPklData ∷ IsCompact → PklData → String
printPklData isCompact (PklData name extendKeys) = unlines $
    [ "[pkl]"
    , "layout             = " ⊕ name
    , "language           = auto"
    , "displayHelpImage   = no"
    , "compactMode        = " ⊕ bool "no" "yes" isCompact
    , "altGrEqualsAltCtrl = no"
    , "suspendTimeOut     = 0"
    , "exitTimeOut        = 0"
    ] ⧺ printExtendKeys' (printPklKeys extendKeys)
  where
    printExtendKeys' [] = []
    printExtendKeys' xs = "":"[extend]":xs


-- TO PKL DATA

toLayoutData ∷ Layout → Logger LayoutData
toLayoutData =
    prepareLayout >>>
    _keys (fmap catMaybes ∘ traverse toSupportedShiftstates) >=>
    toLayoutData'
  where
    toSupportedShiftstates key
      | not (null (view _letters key))
      ∧ all (M.Extend ∈) (view _shiftstates key)
      = pure Nothing
      | otherwise
      = Just <$> filterKeyOnShiftstatesM (supportedShiftstate NotExtend) key

toLayoutData' ∷ Layout → Logger LayoutData
toLayoutData' layout =
    LayoutData
      <$> pure (view _info layout)
      <*> pure "http://pkl.sourceforge.net/"
      <*> runMaybeT (MaybeT (pure extendPos) >>= extendPosToPklPos)
      <*> pure (map winShiftstateFromShiftstate states)
      <*> liftA2 (⧺) pklKeys pklModifierKeys
      <*> (nubOn __pklInt <$> traverse (uncurry deadToPkl) (concatMap (mapMaybe fromPklDead ∘ view _letters) keys))
  where
    (keys, states) = unifyShiftstates (view _keys layout)
    pklKeys = catMaybes <$> traverse (keyToPklKey layout) keys
    pklModifierKeys = catMaybes <$> traverse (singletonKeyToPklKey layout) (view _singletonKeys layout)
    extendPos = listToMaybe $ filterOnSnd (≡ Modifiers Shift [M.Extend]) (view _singletonKeys layout)
    extendPosToPklPos P.CapsLock = pure "CapsLock"
    extendPosToPklPos pos = maybe (e pos) pure (lookup pos posAndString) <|> toPklPos pos
    fromPklDead (CustomDead (Just i) d) = Just (i, d)
    fromPklDead _ = Nothing
    e ∷ Pos → MaybeT Logger α
    e pos = tellMaybeT [show' pos ⊕ " is not supported in PKL"]

deadToPkl ∷ Int → DeadKey → Logger PklDeadKey
deadToPkl i d = PklDeadKey (__dkName d) i <$> charMap
  where
    charMap = traverse (sequenceTuple ∘ (char *** char)) (__stringMap d)
    char ∷ String → Logger Char
    char [x]      = pure x
    char []       = '\0' <$ tell ["empty string in dk" ⊕ show i ⊕ " (" ⊕ __dkName d ⊕ ") in PKL"]
    char xxs@(x:_) = x <$ tell ["the string '" ⊕ xxs ⊕ " is shortened to '" ⊕ [x] ⊕ " in dk" ⊕ show i ⊕ " (" ⊕ __dkName d ⊕ ") in PKL"]

toPklData ∷ Layout → Layout → Logger PklData
toPklData shortcutLayout =
    prepareLayout >>>
    view _keys >>>
    traverse (extendKeyToPklKey shortcutLayout) >$>
    catMaybes >>>
    PklData (view (_info ∘ _name) shortcutLayout)

keyToPklKey ∷ Layout → Key → Logger (Maybe PklKey)
keyToPklKey layout key = runMaybeT $
    PklKey
      <$> toPklPos (view _pos key)
      <*> printShortcutPos (view _shortcutPos key)
      <*> pure (view _capslock key)
      <*> lift (traverse (printLetter NotExtend layout) (view _letters key))
      <*> pure (keyComment (view _pos key) (view _letters key))

extendKeyToPklKey ∷ Layout → Key → Logger (Maybe PklKey)
extendKeyToPklKey layout key = runMaybeT $ do
    letter ← MaybeT $ listToMaybe <$> filterOnSndM (supportedShiftstate Extend) lettersWithStates
    PklSpecialKey
      <$> toPklPos (view _pos key)
      <*> lift (printLetter Extend layout letter)
      <*> pure ""
  where
    lettersWithStates = view _letters key `zip` view _shiftstates key

singletonKeyToPklKey ∷ Layout → SingletonKey → Logger (Maybe PklKey)
singletonKeyToPklKey layout (pos, action) = runMaybeT $
    PklSpecialKey
      <$> toPklPos pos
      <*> lift (printModifierPosition layout action)
      <*> pure (keyComment pos [action])

toPklPos ∷ Pos → MaybeT Logger String
toPklPos P.Alt_R = pure "RAlt"
toPklPos P.Menu = pure "AppsKey"
toPklPos pos = maybe e pure $ printf "SC%03x" <$> lookup pos posAndScancode
  where e = tellMaybeT [show' pos ⊕ " is not supported in PKL"]

keyComment ∷ Pos → [Letter] → String
keyComment pos letters =
    "QWERTY " ⊕ toString pos ⊕ ifNonEmpty (": " ⊕) (intercalate ", " letterComments)
  where
    letterComments = mapMaybe letterComment letters

letterComment ∷ Letter → Maybe String
letterComment = fmap __dkName ∘ letterToDeadKey

printShortcutPos ∷ Pos → MaybeT Logger String
printShortcutPos pos = maybe e pure (lookup pos posAndString)
  where e = MaybeT (Nothing <$ tell [show' pos ⊕ " is not supported in PKL"])

printLetter ∷ IsExtend → Layout → Letter → Logger String
printLetter isExtend _ (Char ' ') = pure (addBraces isExtend "space")
printLetter _ _ (Char c) = pure [c]
printLetter _ _ (Ligature _ x) = pure x
printLetter isExtend layout (Action a) =
    addBraces isExtend <$> actionToPkl layout a
printLetter _ _ (Modifiers Shift [M.Extend]) = pure "--"
printLetter _ _ (Modifiers effect [modifier])
  | effect ≡ defaultModifierEffect modifier
  = case lookup modifier modifierAndPklAction of
      Just (Simple xs) → pure xs
      _ → "--" <$ tell [show' modifier ⊕ " is not supported in PKL"]
  | otherwise = "--" <$ tell [show' modifier ⊕ " with " ⊕ show' effect ⊕ " is not supported in PKL"]
printLetter _ _ (Modifiers _ _) =
     "--" <$ tell ["Multiple modifiers on one letter is not supported in PKL"]
printLetter _ _ (CustomDead (Just i) _) = pure ("dk" ⊕ show i)
printLetter _ _ l@(CustomDead Nothing _) = "--" <$ tell [show' l ⊕ " does not have an dead key number"]
printLetter isExtend layout l@(Redirect mods pos) =
    case getLetterByPosAndShiftstate pos (WP.fromList mods) defaultFullLayout of
      Just l' → addBraces isExtend <$> printPklAction layout l (RedirectLetter l' mods)
      Nothing → "--" <$ tell ["redirecting to " ⊕ show' pos ⊕ " is not supported"]
printLetter _ _ LNothing = pure "--"
printLetter _ _ letter = "--" <$ tell [show' letter ⊕ " is not supported in pkl"]

printPklAction ∷ Layout → Letter → PklAction → Logger String
printPklAction _ _ (Simple x) = pure x
printPklAction layout _ (RedirectLetter letter ms) =
    (("}" ⊕ mapMaybe (`lookup` modifierAndChar) ms ⊕ "{") ⊕) <$> printLetter Extend layout letter

actionToPkl ∷ Layout → Action → Logger String
actionToPkl layout a =
  case lookup a actionAndPklAction of
    Just pklAction → printPklAction layout (Action a) pklAction
    Nothing → "--" <$ tell [show' a ⊕ " is not supported in PKL"]

addBraces ∷ IsExtend → String → String
addBraces _ "--" = "--"
addBraces Extend x = x
addBraces NotExtend x = "={" ⊕ x ⊕ "}"

printModifierPosition ∷ Layout → Letter → Logger String
printModifierPosition layout letter
  | Modifiers _ _ ← letter
  = s <&> \case
      "--" → "disabled"
      s' → s' ⊕ "\tmodifier"
  | otherwise = s
  where
    s = printLetter NotExtend layout letter

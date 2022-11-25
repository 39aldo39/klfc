{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module Layout.Key
    ( BaseChar
    , ActionResult
    , ActionMap
    , DeadKey
    , Letter(..)
    , Key(..)
    , _pos
    , _shortcutPos
    , _shiftlevels
    , _letters
    , _capslock
    , presetDeadKeyToDeadKey
    , letterToChar
    , letterToLigatureString
    , letterToDeadKey
    , letterToCustomDeadKey
    , setCustomDeadKey
    , toIndexedCustomDeadKey
    , setNullChar
    , setDeadNullChar
    , setInternalDeadNullChar
    , addPresetDeadToDead
    , baseCharToChar
    , baseCharToLetter
    , combineKeys
    , combineKeysWithoutOverwrite
    , nubKeys
    , setDefaultShiftstates
    , getLetter
    , getLevel
    , toLettersAndShiftlevels
    , toLettersAndShiftstates
    , filterKeyOnShiftstatesM
    , filterKeyOnShiftstates
    , addCapslock
    ) where

import BasePrelude hiding (toList)
import Prelude.Unicode hiding ((∈), (∉))
import Data.Foldable.Unicode ((∈), (∉))
import Data.Monoid.Unicode ((∅), (⊕))
import qualified Data.Set.Unicode as S
import Util (HumanReadable(..), lensWithDefault, expectedKeys, (!?), (>$>), combineOn, combineWith, nubWithOn, getPrivateChar, split, mapMaybeM)

import qualified Control.Monad.Fail as Fail
import Control.Monad.State (MonadState, state)
import Control.Monad.Writer (runWriter)
import Data.Aeson
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as S
import Lens.Micro.Platform (Lens', makeLenses, view, set, over, _1, _2)

import FileType (FileType)
import Filter (runFilter)
import Layout.Modifier (Modifier, Shiftstate, Shiftlevel, activatedBy, parseJSONShiftlevels)
import qualified Layout.Modifier as M
import Layout.ModifierEffect (ModifierEffect(..), defaultModifierEffect)
import Layout.Pos (Pos)
import Layout.Action (Action)
import Layout.DeadKey (DeadKey'(..), BaseChar'(..), ActionResult'(..), ActionMap', combineDeadKey)
import PresetDeadKey (PresetDeadKey, presetDeadKeyToDeadKey')
import WithBar (WithBar(..))
import WithPlus (WithPlus(..))
import qualified WithPlus as WP

type BaseChar = BaseChar' PresetDeadKey
type ActionResult = ActionResult' Letter PresetDeadKey
type ActionMap = ActionMap' Letter PresetDeadKey
type DeadKey = DeadKey' Letter PresetDeadKey

data Letter
    = Char Char
    | Unicode Char
    | Ligature (Maybe Char) String
    | Action Action
    | Modifiers ModifierEffect [Modifier]
    | Dead PresetDeadKey
    | CustomDead (Maybe Int) DeadKey
    | Redirect [Modifier] Pos
    | LNothing
    deriving (Eq, Ord, Show, Read)

presetDeadKeyToDeadKey ∷ PresetDeadKey → DeadKey
presetDeadKeyToDeadKey = modifyDeadKey ∘ presetDeadKeyToDeadKey'
  where
    modifyDeadKey (DeadKey name baseChar actionMap) = DeadKey name baseChar (map modifyAction actionMap)
    modifyAction (c, OutString s) = (Char c, OutString s)
    modifyAction (c, Next dk) = (Char c, Next (modifyDeadKey dk))

letterToDeadKey ∷ Letter → Maybe DeadKey
letterToDeadKey (CustomDead _ dead) = Just dead
letterToDeadKey (Dead presetDead) = Just (presetDeadKeyToDeadKey presetDead)
letterToDeadKey _ = Nothing

letterToCustomDeadKey ∷ Letter → Maybe DeadKey
letterToCustomDeadKey (CustomDead _ dead) = Just dead
letterToCustomDeadKey _ = Nothing

setCustomDeadKey ∷ [DeadKey] → Letter → Either String Letter
setCustomDeadKey deads (CustomDead i (DeadKey name _ _)) =
  case find ((≡) name ∘ __dkName) deads of
    Just d → Right (CustomDead i d)
    Nothing → Left ("the custom dead key ‘" ⊕ name ⊕ "’ is not defined")
setCustomDeadKey deads (Dead presetDead)
  | Just d ← find ((≡) (toString presetDead) ∘ __dkName) deads
  = Right (CustomDead Nothing d)
setCustomDeadKey _ l = Right l

toIndexedCustomDeadKey ∷ MonadState Int m ⇒ Letter → m Letter
toIndexedCustomDeadKey letter =
  case letterToDeadKey letter of
    Just d → flip CustomDead d ∘ Just <$> state (id &&& succ)
    Nothing → pure letter

setNullChar ∷ MonadState [Char] m ⇒ Letter → m Letter
setNullChar (Ligature Nothing xs) =
    flip Ligature xs ∘ Just <$> getPrivateChar
setNullChar l = setDeadNullChar l

setDeadNullChar ∷ MonadState [Char] m ⇒ Letter → m Letter
setDeadNullChar (CustomDead i (DeadKey name BaseNo lMap)) =
    CustomDead i ∘ flip (DeadKey name) lMap ∘ BaseChar <$> getPrivateChar
setDeadNullChar l = pure l

setInternalDeadNullChar ∷ [DeadKey] → Letter → Letter
setInternalDeadNullChar deads (CustomDead i dead) =
    CustomDead i dead { __actionMap = actionMap }
  where
    actionMap = map (over _1 modifyLetter) (__actionMap dead)
    modifyLetter letter = fromMaybe letter $ do
        DeadKey name _ _ ← letterToDeadKey letter
        CustomDead Nothing <$> find ((≡ name) ∘ __dkName) deads
setInternalDeadNullChar _ l = l

addPresetDeadToDead ∷ Letter → Letter
addPresetDeadToDead (CustomDead i dead@(DeadKey _ (BasePreset p) _)) =
    CustomDead i (fst (runWriter (combineDeadKey dead (presetDeadKeyToDeadKey p))))
addPresetDeadToDead l = l

baseCharToChar ∷ BaseChar → Maybe Char
baseCharToChar BaseNo = Nothing
baseCharToChar (BaseChar c) = Just c
baseCharToChar (BasePreset p) = baseCharToChar (__baseChar (presetDeadKeyToDeadKey p))

baseCharToLetter ∷ BaseChar → Maybe Letter
baseCharToLetter BaseNo = Nothing
baseCharToLetter (BaseChar c) = Just (Char c)
baseCharToLetter (BasePreset p) = Just (Dead p)

letterToChar ∷ Letter → Maybe Char
letterToChar (Char c)       = Just c
letterToChar (Ligature c _) = c
letterToChar _              = Nothing

letterToLigatureString ∷ Letter → Maybe String
letterToLigatureString (Ligature _ xs) = Just xs
letterToLigatureString _               = Nothing

letterToString ∷ Letter → String
letterToString (Char x) = [x]
letterToString (Unicode x) = printf "U+%04X" x
letterToString (Ligature Nothing xs) = "lig:" ⊕ xs
letterToString (Ligature (Just c) xs) = c:':':xs
letterToString (Modifiers effect [modifier]) | defaultModifierEffect modifier ≡ effect = toString modifier
letterToString (Modifiers Shift mods) = "shift:" ⊕ intercalate "," (map toString mods)
letterToString (Modifiers Latch mods) = "latch:" ⊕ intercalate "," (map toString mods)
letterToString (Modifiers Lock  mods) = "lock:"  ⊕ intercalate "," (map toString mods)
letterToString (Action x) = toString x
letterToString (Redirect []   p) = "red:" ⊕ toString p
letterToString (Redirect mods p) = intercalate "+" (map toString mods ⧺ [toString p])
letterToString (Dead x) = toString x
letterToString (CustomDead _ d) = "cdk:" ⊕ __dkName d
letterToString LNothing = ""

letterFromString ∷ MonadFail m ⇒ String → m Letter
letterFromString s = maybe e pure ∘ asum ∘ fmap ($ s) $
    [ stripPrefix "lig:" >$> ligature'
    , stripPrefix "ligature:" >$> ligature'
    , stripPrefix "dk:" >=> dead
    , stripPrefix "dead:" >=> dead
    , stripPrefix "deadKey:" >=> dead
    , stripPrefix "cdk:" >$> customDead
    , stripPrefix "customDead:" >$> customDead
    , stripPrefix "customDeadKey:" >$> customDead
    , stripPrefix "red:" >=> redirect
    , stripPrefix "redirect:" >=> redirect
    , stripPrefix "shift:" >=> modifiersWith Shift
    , stripPrefix "latch:" >=> modifiersWith Latch
    , stripPrefix "lock:" >=> modifiersWith Lock
    , char
    , unicode
    , ligature
    , modifier
    , action
    , dead
    , redirect
    , nothing
    ]
  where
    e = Fail.fail ("‘" ⊕ s ⊕ "’ is not a valid letter")
    char [x] = Just (Char x)
    char _   = Nothing
    unicode ('U':'+':xs) = Unicode ∘ chr <$> readMaybe ("0x" ⊕ xs)
    unicode _            = Nothing
    ligature (c:':':xs) = Just (Ligature (Just c) xs)
    ligature _          = Nothing
    ligature' = Ligature Nothing
    modifiersWith effect =
        toList ∘ split (∈ [',', ' ']) >>>
        traverse parseString ∘ filter (not ∘ null) >$>
        Modifiers effect
    modifier = parseString >$> \m → Modifiers (defaultModifierEffect m) [m]
    customDead xs = CustomDead Nothing (DeadKey xs BaseNo [])
    action = parseString >$> Action
    dead = parseString >$> Dead
    redirect =
        split (≡'+') >>>
        (traverse parseString ∘ NE.init &&& parseString ∘ NE.last) >>>
        uncurry (liftA2 Redirect)
    nothing = bool Nothing (Just LNothing) ∘ null

instance HumanReadable Letter where
    typeName _ = "letter"
    toString = letterToString
    parseString = letterFromString
instance ToJSON Letter where
    toJSON = hrToJSON
instance FromJSON Letter where
    parseJSON = hrParseJSON

data Key = Key
    { __pos ∷ Pos
    , keyShortcutPos ∷ Maybe Pos
    , __shiftlevels ∷ [Shiftlevel]
    , __letters ∷ [Letter]
    , keyCapslock ∷ Maybe Bool
    } deriving (Show, Read)
makeLenses ''Key
_shortcutPos ∷ Lens' Key Pos
_shortcutPos = lensWithDefault guess (\y x → x {keyShortcutPos = y}) keyShortcutPos
  where guess = liftA2 shortcutPosGuess __pos id
_capslock ∷ Lens' Key Bool
_capslock = lensWithDefault guess (\y x → x {keyCapslock = y}) keyCapslock
  where guess = capslockGuess

shortcutPosGuess ∷ Pos → Key → Pos
shortcutPosGuess p = fromMaybe p <<<
    flip getLetter (∅) >>> letterToChar >=> toUpper >>> (:[]) >>> parseString

capslockGuess ∷ Key → Bool
capslockGuess = fromMaybe False <<<
    flip getLetter (∅) >>> letterToChar >$> isAlpha

instance ToJSON Key where
    toJSON (Key pos maybeShortcutPos shiftlevels letters maybeCapslock) = object $
        [ "pos"         .= pos ] ⧺
        [ "shortcutPos" .= shortcutPos | shortcutPos ← maybeToList maybeShortcutPos ] ⧺
        [ "shiftlevels" .= shiftlevels | not (null shiftlevels) ] ⧺
        [ "letters"     .= letters     | not (null letters) ] ⧺
        [ "capslock"    .= capslock    | capslock ← maybeToList maybeCapslock ]

instance FromJSON (FileType → Maybe Key) where
    parseJSON = withObject "key" $ \o → do
        expectedKeys ["filter","pos","shortcutPos","shiftlevels","shiftstates","letters","capslock"] o
        filt         ← o .:? "filter"  .!= (∅)
        pos          ← o .:  "pos"
        shortcutPos  ← o .:? "shortcutPos"
        shiftlevels' ← fromMaybe (pure []) (parseJSONShiftlevels o)
        letters'     ← o .:? "letters" .!= []
        capslock     ← o .:? "capslock"
        let (shiftlevels, letters) = nubShiftlevels shiftlevels' letters'
        let key = Key pos shortcutPos shiftlevels letters capslock
        pure (bool Nothing (Just key) ∘ runFilter filt)
      where
        nubShiftlevels [] letters = ([], letters)
        nubShiftlevels shiftlevels letters =
            unzip $ nubBy ((≡) `on` fst) (zip shiftlevels letters)

combineKeys ∷ [Key] → [Key] → [Key]
combineKeys = combineOn (foldl' combineKey) (view _pos)

combineKeysWithoutOverwrite ∷ [Key] → [Key] → [Key]
combineKeysWithoutOverwrite = combineOn (foldr combineKey) (view _pos)

nubKeys ∷ [Key] → [Key]
nubKeys = nubWithOn (foldl' combineKey) (view _pos)

combineKey ∷ Key → Key → Key
combineKey (Key p sp1 ss1 ls1 cl1) (Key _ sp2 ss2 ls2 cl2) =
    Key p (sp2 <|> sp1) ss ls (cl2 <|> cl1)
  where
    (ss, ls) = unzip $ combineLetters (zip ss1 ls1) (zip ss2 ls2)

combineLetters ∷ [(Shiftlevel, Letter)] → [(Shiftlevel, Letter)] → [(Shiftlevel, Letter)]
combineLetters = combineWith (combineShfiftlevels ∘: (:)) ((not ∘ null) ∘: (intersect `on` toList ∘ fst))
  where (∘:) = (∘) ∘ (∘)

combineShfiftlevels ∷ [(Shiftlevel, Letter)] → [(Shiftlevel, Letter)]
combineShfiftlevels [] = []
combineShfiftlevels ((level, letter) : ls) =
    case filter (\shiftstate → all (shiftstate ∉) (map fst ls)) (toList level) of
      [] → ls'
      (x:xs) → (WithBar (x :| xs), letter) : ls'
  where
    ls' = combineShfiftlevels ls

setDefaultShiftstates ∷ [Shiftlevel] → Key → Key
setDefaultShiftstates levels key
  | null (view _shiftlevels key) = set _shiftlevels (zipWith const levels (view _letters key)) key
  | otherwise = key

getLetter ∷ Key → Shiftstate → Letter
getLetter key = fromMaybe LNothing ∘
    (getLevel key >=> (view _letters key !?) ∘ fst)

getLevel ∷ Key → Shiftstate → Maybe (Int, Shiftstate)
getLevel key (WithPlus mods) = over _2 WithPlus <$> getLevel' key mods

getLevel' ∷ Key → Set Modifier → Maybe (Int, Set Modifier)
getLevel' key mods =
  case findIndex (activatedBy mods) (view _shiftlevels key) of
    Just i  → Just (i, mods S.∩ S.fromList M.controlMods)
    Nothing → reducedLevel
  where
    reducedLevel
      | null mods = Nothing
      | view _capslock key ∧ M.CapsLock ∈ mods = over _2 (S.delete M.Shift) <$>
          getLevel' key (mods S.∆ S.fromList [M.Shift, M.CapsLock])
      | otherwise = ($ mods) $
          S.deleteFindMin >>>
          sequence ∘ (S.insert *** getLevel' key) >$>
          uncurry (over _2)

toLettersAndShiftlevels ∷ Key → [(Letter, Shiftlevel)]
toLettersAndShiftlevels = uncurry zip ∘ (view _letters &&& view _shiftlevels)

toLettersAndShiftstates ∷ Key → [(Letter, Shiftstate)]
toLettersAndShiftstates =
    toLettersAndShiftlevels >>>
    concatMap (uncurry zip ∘ (repeat *** toList))

filterKeyOnShiftstatesM ∷ Monad m ⇒ (Shiftstate → m Bool) → Key → m Key
filterKeyOnShiftstatesM p key = do
    (ls, ms) ← unzip <$> mapMaybeSndM p' (view _letters key `zip` view _shiftlevels key)
    pure ∘ set _letters ls ∘ set _shiftlevels ms $ key
  where
    p' = filterM p ∘ toList >$> fmap WithBar ∘ NE.nonEmpty
    mapMaybeSndM ∷ Monad m ⇒ (β → m (Maybe γ)) → [(α, β)] → m [(α, γ)]
    mapMaybeSndM f = mapMaybeM (fmap sequence ∘ traverse f)

filterKeyOnShiftstates ∷ (Shiftstate → Bool) → Key → Key
filterKeyOnShiftstates p = runIdentity ∘ filterKeyOnShiftstatesM (pure ∘ p)

addCapslock ∷ Key → Key
addCapslock key
  | any (any (M.CapsLock ∈)) (view _shiftlevels key) = key
  | not (view _capslock key) =
      let newLevels = map (fmap (⊕ WP.singleton M.CapsLock)) (view _shiftlevels key)
      in  over _letters (\ls → ls ⧺ ls) ∘ over _shiftlevels (⧺ newLevels) $ key
  | otherwise =
      let newStates = map (⊕ WP.singleton M.CapsLock) (concatMap toList (view _shiftlevels key))
          newLevels = map (WithBar ∘ (:| [])) newStates
          newLetters = map (getLetter key) newStates
      in  over _letters (⧺ newLetters) ∘ over _shiftlevels (⧺ newLevels) $ key

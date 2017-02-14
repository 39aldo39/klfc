{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Layout.Key
    ( Letter(..)
    , Key(..)
    , _pos
    , _shortcutPos
    , _shiftstates
    , _letters
    , _capslock
    , letterToChar
    , letterToLigatureString
    , letterToDeadKey
    , letterToCustomDeadKey
    , setCustomDeadKey
    , toIndexedCustomDeadKey
    , setNullChar
    , combineKeys
    , combineKeysWithoutOverwrite
    , nubKeys
    , setDefaultShiftstates
    , getLetter
    , getLevel
    , filterKeyOnShiftstatesM
    , filterKeyOnShiftstates
    , addCapslock
    ) where

import BasePrelude
import Prelude.Unicode hiding ((∈))
import Data.Foldable.Unicode ((∈))
import Data.Monoid.Unicode ((∅), (⊕))
import qualified Data.Set.Unicode as S
import Util (HumanReadable(..), lensWithDefault, expectedKeys, (!?), (>$>), combineWithOn, nubWithOn, split)

import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
import Control.Monad.State (State, state)
import Data.Aeson
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as S
import Lens.Micro.Platform (Lens', makeLenses, view, set, over, _2)

import FileType (FileType)
import Filter (Filter(..))
import Layout.Modifier (Modifier, Shiftstate, activatedBy)
import qualified Layout.Modifier as M
import Layout.ModifierEffect (ModifierEffect(..), defaultModifierEffect)
import Layout.Pos (Pos)
import Layout.Action (Action)
import Layout.DeadKey (DeadKey(DeadKey), __dkName)
import PresetDeadKey (PresetDeadKey, presetDeadKeyToDeadKey)
import WithPlus (WithPlus(..))
import qualified WithPlus as WP

data Letter
    = Char Char
    | Ligature (Maybe Char) String
    | Action Action
    | Modifiers ModifierEffect [Modifier]
    | Dead PresetDeadKey
    | CustomDead (Maybe Int) DeadKey
    | Redirect [Modifier] Pos
    | LNothing
    deriving (Eq, Show, Read)

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
setCustomDeadKey _ l = Right l

toIndexedCustomDeadKey ∷ Letter → State Int Letter
toIndexedCustomDeadKey letter =
  case letterToDeadKey letter of
    Just d → flip CustomDead d ∘ Just <$> state (id &&& succ)
    Nothing → pure letter

setNullChar ∷ Letter → State [Char] Letter
setNullChar (Ligature Nothing xs) =
    flip Ligature xs ∘ Just <$> state (fromMaybe e ∘ uncons)
  where e = error "too many ligatures"
setNullChar (CustomDead i (DeadKey name Nothing lMap)) =
    CustomDead i ∘ flip (DeadKey name) lMap ∘ Just <$> state (fromMaybe e ∘ uncons)
  where e = error "too many dead keys"
setNullChar l = pure l

letterToChar ∷ Letter → Maybe Char
letterToChar (Char c)       = Just c
letterToChar (Ligature c _) = c
letterToChar _              = Nothing

letterToLigatureString ∷ Letter → Maybe String
letterToLigatureString (Ligature _ xs) = Just xs
letterToLigatureString _               = Nothing

letterToString ∷ Letter → String
letterToString (Char x) = [x]
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
    ligature (c:':':xs) = Just (Ligature (Just c) xs)
    ligature _          = Nothing
    ligature' = Ligature Nothing
    modifiersWith effect =
        toList ∘ split (∈ [',', ' ']) >>>
        traverse parseString ∘ filter (not ∘ null) >$>
        Modifiers effect
    modifier = parseString >$> \m → Modifiers (defaultModifierEffect m) [m]
    customDead xs = CustomDead Nothing (DeadKey xs Nothing [])
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
    , __shiftstates ∷ [Shiftstate]
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
shortcutPosGuess p =
    (flip getLetter (∅) >>> letterToChar >=> toUpper >>> (:[]) >>> parseString) >>> fromMaybe p

capslockGuess ∷ Key → Bool
capslockGuess = (flip getLetter (∅) >>> letterToChar >$> isAlpha) >>> fromMaybe False

instance ToJSON Key where
    toJSON (Key pos maybeShortcutPos shiftstates letters maybeCapslock) = object $
        [ "pos"         .= pos ] ⧺
        [ "shortcutPos" .= shortcutPos | shortcutPos ← maybeToList maybeShortcutPos ] ⧺
        [ "shiftstates" .= shiftstates | not (null shiftstates) ] ⧺
        [ "letters"     .= letters     | not (null letters) ] ⧺
        [ "capslock"    .= capslock    | capslock ← maybeToList maybeCapslock ]

instance FromJSON (FileType → Maybe Key) where
    parseJSON = withObject "key" $ \o → do
        expectedKeys ["filter","pos","shortcutPos","shiftstates","letters","capslock"] o
        filt         ← o .:? "filter"      .!= Filter (const True)
        pos          ← o .:  "pos"
        shortcutPos  ← o .:? "shortcutPos"
        shiftstates' ← o .:? "shiftstates" .!= []
        letters'     ← o .:? "letters"     .!= []
        capslock     ← o .:? "capslock"
        let (shiftstates, letters) = nubShiftstates shiftstates' letters'
        let key = Key pos shortcutPos shiftstates letters capslock
        pure (bool Nothing (Just key) ∘ runFilter filt)
      where
        nubShiftstates [] letters = ([], letters)
        nubShiftstates shiftstates letters =
            unzip $ nubBy ((≡) `on` fst) (zip shiftstates letters)

combineKeys ∷ [Key] → [Key] → [Key]
combineKeys = combineWithOn (foldl' combineKey) (view _pos)

combineKeysWithoutOverwrite ∷ [Key] → [Key] → [Key]
combineKeysWithoutOverwrite = combineWithOn (foldr combineKey) (view _pos)

nubKeys ∷ [Key] → [Key]
nubKeys = nubWithOn (foldl' combineKey) (view _pos)

combineKey ∷ Key → Key → Key
combineKey (Key p sp1 ss1 ls1 cl1) (Key _ sp2 ss2 ls2 cl2) =
    Key p (sp2 <|> sp1) ss ls (cl2 <|> cl1)
  where
    (ss, ls) = unzip $ combineLetters (zip ss1 ls1) (zip ss2 ls2)

combineLetters ∷ [(Shiftstate, Letter)] → [(Shiftstate, Letter)] → [(Shiftstate, Letter)]
combineLetters = combineWithOn (\(ss,l) ls → (ss, NE.last (l :| map snd ls))) fst

setDefaultShiftstates ∷ [Shiftstate] → Key → Key
setDefaultShiftstates states key
  | null (view _shiftstates key) = set _shiftstates (zipWith const states (view _letters key)) key
  | otherwise = key

getLetter ∷ Key → Shiftstate → Letter
getLetter key = fromMaybe LNothing ∘
    (getLevel (view _shiftstates key) >=> (view _letters key !?) ∘ fst)

getLevel ∷ [Shiftstate] → Shiftstate → Maybe (Int, Shiftstate)
getLevel states (WithPlus mods) = over _2 WithPlus <$> getLevel' states mods

getLevel' ∷ [Shiftstate] → Set Modifier → Maybe (Int, Set Modifier)
getLevel' states mods =
  case findIndex (activatedBy mods) states of
    Just i  → Just (i, mods S.∩ S.fromList M.controlMods)
    Nothing → reducedLevel
  where
    reducedLevel
      | null mods = Nothing
      | M.CapsLock ∈ mods = over _2 (S.delete M.Shift) <$>
          getLevel' states (mods S.∆ S.fromList [M.Shift, M.CapsLock])
      | otherwise = ($ mods) $
          S.deleteFindMin >>>
          sequence ∘ (S.insert *** getLevel' states) >$>
          uncurry (over _2)

filterKeyOnShiftstatesM ∷ Monad m ⇒ (Shiftstate → m Bool) → Key → m Key
filterKeyOnShiftstatesM p key = do
  (ls, ms) ← unzip <$> filterM (p ∘ snd) (view _letters key `zip` view _shiftstates key)
  pure ∘ set _letters ls ∘ set _shiftstates ms $ key

filterKeyOnShiftstates ∷ (Shiftstate → Bool) → Key → Key
filterKeyOnShiftstates p = runIdentity ∘ filterKeyOnShiftstatesM (pure ∘ p)

addCapslock ∷ Key → Key
addCapslock key
  | any (M.CapsLock ∈) (view _shiftstates key) = key
  | otherwise = over _letters (⧺ newLetters) ∘ over _shiftstates (⧺ newStates) $ key
  where
    newStates = map (⊕ WP.singleton M.CapsLock) (view _shiftstates key)
    newLetters
      | view _capslock key = map (getLetter key) newStates
      | otherwise = view _letters key

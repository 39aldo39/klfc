{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Layout.Layout
    ( Information(Information)
    , _fullName
    , _name
    , _copyright
    , _company
    , _localeId
    , _version
    , _description
    , Layout(Layout)
    , SingletonKey
    , _info
    , _singletonKeys
    , _mods
    , _keys
    , isEmptyLayout
    , setNullChars
    , layoutOrd
    , layoutDelims
    , applyModLayout
    , getDefaultKeys
    , addDefaultKeysWith
    , addDefaultKeys
    , removeEmptyLetters
    , unifyShiftstates
    , getLevel
    , getLetterByPosAndShiftstate
    , getPosByLetterAndShiftstate
    ) where

import BasePrelude
import Prelude.Unicode hiding ((∈))
import Data.Foldable.Unicode ((∈))
import Data.Monoid.Unicode ((∅), (⊕))
import qualified Data.Set.Unicode as S
import Util (parseString, lensWithDefault', expectedKeys, combineWithOn, nubWithOn, privateChars, mconcatMapM, (!?))

import Control.Monad.State (evalState)
import Data.Aeson
import Data.Aeson.TH (deriveJSON, fieldLabelModifier, omitNothingFields)
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T (Text, unpack)
import Data.Text.Lazy.Builder (Builder)
import Lens.Micro.Platform (Lens', view, set, over, makeLenses, _2)

import FileType (FileType)
import Filter (Filter(..))
import JsonPretty (keyOrder', delimsFromList)
import Layout.DeadKey (DeadKey)
import Layout.Key
import Layout.Mod (Mod(Mod), applyMod)
import Layout.Modifier (Modifier, Shiftstate)
import qualified Layout.Modifier as M
import Layout.Pos (Pos)
import WithPlus (WithPlus(..))

data Information = Information
    { infoFullName, infoName, __copyright, __company
    , __localeId, __version, __description ∷ Maybe String
    } deriving (Show, Read)
makeLenses ''Information
_fullName ∷ Lens' Information String
_fullName = lensWithDefault' "Custom Keyboard Layout" (\y x → x {infoFullName = y}) infoFullName
_name ∷ Lens' Information String
_name = lensWithDefault' "custom" (\y x → x {infoName = y}) infoName

infoKeys ∷ [T.Text]
infoKeys = ["fullName","name","copyright","company","localeId","version","description"]

isEmptyInformation ∷ Information → Bool
isEmptyInformation (Information Nothing Nothing Nothing Nothing Nothing Nothing Nothing) = True
isEmptyInformation _ = False

instance Monoid Information where
    mempty = Information Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    mappend (Information a1 b1 c1 d1 f1 g1 h1)
            (Information a2 b2 c2 d2 f2 g2 h2) =
        Information (a1 ⊕ a2) (b1 ⊕ b2) (c1 ⊕ c2) (d1 ⊕ d2)
                    (f1 ⊕ f2) (g1 ⊕ g2) (h1 ⊕ h2)

$(deriveJSON defaultOptions
    { fieldLabelModifier = \s →
        case stripPrefix "info" s of
          Just (x:xs) → toLower x : xs
          _ → dropWhile (≡'_') s
    , omitNothingFields = True
    } ''Information)

type SingletonKey = (Pos, Letter)
data Layout = Layout
    { __info ∷ Information
    , __singletonKeys ∷ [SingletonKey]
    , __mods ∷ [Mod]
    , __keys ∷ [Key]
    } deriving (Show, Read)
makeLenses ''Layout

layoutKeys ∷ [T.Text]
layoutKeys = ["singletonKeys","qwertyShortcuts","mods","keysWithShiftstates","shiftstates","keys","customDeadKeys"]

isEmptyLayout ∷ Layout → Bool
isEmptyLayout (Layout info [] [] []) = isEmptyInformation info
isEmptyLayout _ = False

instance Monoid Layout where
    mempty = Layout (∅) (∅) (∅) []
    mappend (Layout a1 b1 c1 keys1)
            (Layout a2 b2 c2 keys2) =
        Layout (a1 ⊕ a2) (b1 `combineSingletonKeys` b2)
               (c1 ⊕ c2) (keys1 `combineKeys` keys2)

nubSingletonKeys ∷ [SingletonKey] → [SingletonKey]
nubSingletonKeys = nubWithOn (\(pos,l) ks → (pos, NE.last (l :| map snd ks))) fst

combineSingletonKeys ∷ [SingletonKey] → [SingletonKey] → [SingletonKey]
combineSingletonKeys = combineWithOn (\(pos,l) ks → (pos, NE.last (l :| map snd ks))) fst

unifyShiftstates ∷ [Key] → ([Key], [Shiftstate])
unifyShiftstates = flip map <*> setStates ∘ states &&& states
  where
    states = nub ∘ concatMap NE.head ∘ NE.group ∘ map (view _shiftstates)

setStates ∷ [Shiftstate] → Key → Key
setStates states key
  | states ≡ view _shiftstates key = key
  | otherwise = set _shiftstates states ∘ set _letters letters $ key
  where
    letters = map (\s → maybe LNothing snd (find ((≡) s ∘ fst) xs)) states
    xs = view _shiftstates key `zip` view _letters key

instance ToJSON Layout where
    toJSON (Layout info singletonKeys mods keys) =
        let hmInfo = fromValue (∅) $ toJSON info
            hmKeys = fromValue (∅) $ toJSON' keys
            customDeadKeys = concatMap (mapMaybe letterToCustomDeadKey ∘ view _letters) keys
            hmSingletonKeys  = HM.fromList [ "singletonKeys"  .= singletonKeys  | not (null singletonKeys) ]
            hmMods           = HM.fromList [ "mods"           .= mods           | not (null mods) ]
            hmCustomDeadKeys = HM.fromList [ "customDeadKeys" .= customDeadKeys | not (null customDeadKeys) ]
        in
        Object (HM.unions [hmInfo, hmKeys, hmSingletonKeys, hmMods, hmCustomDeadKeys])
      where
        fromValue _ (Object hm) = hm
        fromValue x _           = x

toJSON' ∷ [Key] → Value
toJSON' keys =
  case NE.groupBy ((≡) `on` view _shiftstates) keys of
    []   → object []
    [ks] → keysToJSON ks
    kss  → object ["keysWithShiftstates" .= map keysToJSON kss]
  where
    keysToJSON (k :| ks) = object ["shiftstates" .= view _shiftstates k, "keys" .= toJSON (k:ks)]

instance FromJSON (FileType → Layout) where
    parseJSON = withObject "layout" $ \o → do
        expectedKeys ("filter" : infoKeys ⧺ layoutKeys) o
        filt ← o .:? "filter" .!= Filter (const True)
        info ← parseJSON (Object o)
        customDeadKeys  ← o .:? "customDeadKeys"  .!= []
        qwertyShortcuts ← o .:? "qwertyShortcuts" .!= False
        singletonKeys ← nubSingletonKeys <$> o .:? "singletonKeys" .!= []
        keys ← (fmap ∘ fmap)
                   (nubKeys >>>
                    either error id ∘ setCustomDeads customDeadKeys >>>
                    bool id (map (liftA2 (set _shortcutPos) (view _pos) id)) qwertyShortcuts
                   ) (parseJSON' (Object o))
        layout ← Layout info singletonKeys <$> o .:? "mods" .!= []
        pure (bool (∅) ∘ layout ∘ keys <*> runFilter filt)

parseJSON' ∷ Value → Parser (FileType → [Key])
parseJSON' = withObject "keys" (lift3A2 (⧺) parseKeys parseKeysWithShiftstates)
  where
    lift3A2 = liftA2 ∘ liftA2 ∘ liftA2
    parseKeysWithShiftstates o =
        o .:? "keysWithShiftstates" .!= [] >>= mconcatMapM parseJSON'
    parseKeys o = do
        states' ← o .:? "shiftstates" ∷ Parser (Maybe [Shiftstate])
        keys' ← o .:? "keys" ∷ Parser (Maybe [FileType → Maybe Key])
        case (states', keys') of
          (Just states, Just keys) → (pure ∘ (fmap ∘ map) (setDefaultShiftstates states) ∘ flip (mapMaybe ∘ flip ($))) keys
          (Nothing, Just _) → fail "object with property keys, but without corresponding property shiftstates"
          (_, Nothing) → pure (∅)

getDefaultKeys ∷ [Key] → Layout → [Key]
getDefaultKeys = flip $ \layout → filter ((∈ posses layout) ∘ view _pos)
  where
    posses = map (view _pos) ∘ view _keys

addDefaultKeysWith ∷ ([Key] → Layout → [Key]) → [Key] → Layout → Layout
addDefaultKeysWith f defaultKeys layout =
    over _keys (`combineKeysWithoutOverwrite` f defaultKeys layout) layout

addDefaultKeys ∷ [Key] → Layout → Layout
addDefaultKeys = addDefaultKeysWith getDefaultKeys

removeEmptyLetters ∷ Key → Key
removeEmptyLetters key =
    let (ls, ss) = unzip $ dropWhileEnd ((≡) LNothing ∘ fst) (view _letters key `zip` view _shiftstates key)
    in  set _letters ls ∘ set _shiftstates ss $ key

setCustomDeads ∷ [DeadKey] → [Key] → Either String [Key]
setCustomDeads = traverse ∘ _letters ∘ traverse ∘ setCustomDeadKey

setNullChars ∷ [Key] → [Key]
setNullChars = flip evalState privateChars ∘ (traverse ∘ _letters ∘ traverse) setNullChar

layoutOrd ∷ T.Text → T.Text → Ordering
layoutOrd = keyOrder'
    ["fullName","name","copyright","company","localName","localeId","version","description"
    ,"qwertyShortcuts","singletonKeys"
    ,"pos","shortcutPos","shiftstates","letters","capslock"
    ,"keys","keysWithShiftstates","customDeadKeys","mods"
    ,"baseChar","stringMap"
    ]
    (parseString ∘ T.unpack ∷ T.Text → Maybe Pos)

layoutDelims ∷ T.Text → [Builder]
layoutDelims = delimsFromList
    [ ("singletonKeys", ["\n", " "])
    , ("shiftstates", [" ", " "])
    , ("keys", ["\n", " ", " "])
    , ("customDeadKeys", ["\n", "\n", "\n", " "])
    , ("mods", ["\n", "\n", "\n", " "])
    ]

applyModLayout ∷ Mod → Layout → Layout
applyModLayout layoutMod | layoutMod ≡ (∅) = id
applyModLayout layoutMod@(Mod nameM _) =
    over (_info ∘ _fullName) (⊕ (" (" ⊕ nameM ⊕ ")")) ∘
    over (_info ∘ _name) (⊕ ('_':nameM)) ∘
    over (_keys ∘ traverse ∘ _pos) (applyMod layoutMod)

getLevel ∷ Key → Shiftstate → (Int, Shiftstate)
getLevel key (WithPlus mods) = over _2 WithPlus (getLevel' key mods)

getLevel' ∷ Key → S.Set Modifier → (Int, S.Set Modifier)
getLevel' key mods =
  case elemIndex (WithPlus mods) (view _shiftstates key) of
    Just i  → (i, mods S.∩ S.fromList M.controlMods)
    Nothing → reducedLevel
  where
    reducedLevel
      | null mods = (0, (∅))
      | M.CapsLock ∈ mods = over _2 (S.delete M.Shift) $ getLevel' key (mods S.∆ S.fromList [M.Shift, M.CapsLock])
      | otherwise = uncurry (over _2) $ (S.insert *** getLevel' key) (S.deleteFindMin mods)

getLetterByPosAndShiftstate ∷ Pos → Shiftstate → Layout → Maybe Letter
getLetterByPosAndShiftstate pos state =
    listToMaybe ∘ mapMaybe toLetter ∘ filter ((≡ pos) ∘ view _pos) ∘ view _keys
  where
    toLetter key = view _letters key !? fst (getLevel key state)

getPosByLetterAndShiftstate ∷ Letter → Shiftstate → Layout → [Pos]
getPosByLetterAndShiftstate letter state =
    map (view _pos) ∘ filter ((≡ Just letter) ∘ toLetter) ∘ view _keys
  where
    toLetter key = view _letters key !? fst (getLevel key state)

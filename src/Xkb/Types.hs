{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Xkb.Types where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((⊕))
import qualified Data.Set.Unicode as S
import Util (toString, subsets, (>$>))
import WithPlus (WithPlus(..))

import Control.Monad.Reader (MonadReader)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as S

import Layout.Key (letterToChar, getLevel)
import qualified Layout.Modifier as M
import Layout.Types
import Lens.Micro.Platform (view)
import Lookup.Linux (presetTypes, defaultTypes, modifierAndTypeModifier)
import Xkb.General (XkbConfig, prepareLayout, supportedTypeModifier)

isRightGuess ∷ Key → Bool
isRightGuess key = rightGuessMods ∧ rightCapslock
  where
    rightGuessMods = view _shiftlevels key ≡ guessMods
    rightCapslock = maybe True (≡ view _capslock key) guessAlph
    (guessMods, guessAlph) =
      case fromMaybe (Left 0) (Map.lookup (view _pos key) defaultTypes) of
        Left size →
            if size > length (view _letters key)
                then (repeat M.empty, Nothing)
                else guessType (view _letters key)
        Right x → x

guessType ∷ [Letter] → ([Shiftlevel], Maybe Bool)
guessType []         = ([], Nothing)
guessType [_]        = ([M.empty], Nothing)
guessType [l,l']     = ([M.empty, M.singleton M.Shift], Just (isCapslock l l'))
guessType [l,l',_]   = ([M.empty, M.singleton M.Shift, M.singleton M.AltGr], Just (isCapslock l l'))
guessType [l,l',_,_] = ([M.empty, M.singleton M.Shift, M.singleton M.AltGr, M.fromList [M.Shift,M.AltGr]], Just (isCapslock l l'))
guessType _    = (repeat M.empty, Nothing)

-- http://www.charvolant.org/~doug/xkb/html/node5.html#SECTION00054000000000000000
isCapslock ∷ Letter → Letter → Bool
isCapslock l1 l2 = maybe False isLower (letterToChar l1)
                 ∧ maybe False isUpper (letterToChar l2)

keytypeName ∷ Key → String
keytypeName = (\x → fromMaybe x (lookup x presetTypes)) ∘ keytypeName'

keytypeName' ∷ Key → String
keytypeName' key =
    (intercalate "_" ∘ map (map toUpper ∘ toString) $ view _shiftlevels key) ⊕
    bool "" "_ALPHABETIC" (view _capslock key)

printTypes ∷ (Logger m, MonadReader XkbConfig m) ⇒ Layout → m String
printTypes = prepareLayout >=> \layout → do
    types ← getTypes layout
    let virtualMods = S.unions (map __typeMods types)
    pure $ unlines $
        [ "default xkb_types \"basic\" {"
        ] ⧺ printVirtualMods (filter isVirtualModifier (S.toList virtualMods)) ⧺
        concatMap printType types ⧺
        [ "};"
        ]
  where
    isVirtualModifier = (∈ [M.AltGr, M.Extend, M.Win, M.NumLock])
    printVirtualMods [] = [""]
    printVirtualMods xs =
        [ ""
        , "    virtual_modifiers " ⊕ intercalate "," (mapMaybe (`lookup` modifierAndTypeModifier) xs) ⊕ ";"
        , ""
        ]

data Type = Type
    { __typeName   ∷ String
    , __typeMods   ∷ Set Modifier
    , __maps       ∷ [(Shiftstate, Int)]
    , __preserves  ∷ [(Shiftstate, Shiftstate)]
    , __levelNames ∷ [(Int, String)]
    } deriving (Eq, Show, Read)

printType ∷ Type → [String]
printType (Type typeName typeMods maps preserves levelNames) = map (replicate 4 ' ' ⊕) $
    [ "type " ⊕ show typeName ⊕ " {"
    ] ⧺ map (replicate 4 ' ' ⊕)
    ( maybe [] ((:[]) ∘ ("modifiers = " ⊕) ∘ (⊕ ";")) (listMods (S.toDescList typeMods))
    ⧺ bool [""] [] (null maps)
    ⧺ map printComb maps
    ⧺ bool [""] [] (null preserves)
    ⧺ map printPreserve preserves
    ⧺ bool [""] [] (null levelNames)
    ⧺ map printName levelNames
    ) ⧺
    [ "};"
    , ""
    ]
  where
    printComb (WithPlus mods, level) = "map[" ⊕ fromMaybe "None" (listMods (S.toDescList mods)) ⊕ "] = Level" ⊕ show (succ level) ⊕ ";"
    printPreserve (WithPlus mods, WithPlus preserveMods) = "preserve[" ⊕ fromMaybe "None" (listMods (S.toDescList mods)) ⊕ "] = " ⊕ fromMaybe "None" (listMods (S.toDescList preserveMods)) ⊕ ";"
    printName (level, string) = "level_name[Level" ⊕ show (succ level) ⊕ "] = \"" ⊕ string ⊕ "\";"
    listMods [] = Nothing
    listMods ms = Just ∘ intercalate "+" ∘ mapMaybe (`lookup` modifierAndTypeModifier) $ ms

getTypes ∷ Logger m ⇒ Layout → m [Type]
getTypes =
    traverse getType ∘ view _keys >$>
    filter (not ∘ isPresetType ∘ __typeName) >>>
    nub
  where
    isPresetType = (∈ map snd presetTypes)

getType ∷ Logger m ⇒ Key → m Type
getType key = getType' key <$> filterMS supportedTypeModifier mods
  where
    mods       = (S.∪ extraMods) ∘ S.unions ∘ map getSet ∘ concatMap toList ∘ view _shiftlevels $ key
    extraMods  = S.fromList [M.CapsLock | view _capslock key]
    filterMS f = fmap S.fromAscList ∘ filterM f ∘ S.toAscList

getType' ∷ Key → Set Modifier → Type
getType' key mods = Type
    { __typeName   = keytypeName key
    , __typeMods   = mods
    , __maps       = zip subMods xkbLevels
    , __preserves  = filter (not ∘ null ∘ snd) (zip subMods ignoredMods)
    , __levelNames = zip [0..] (map (showLevel ∘ toList) states)
    }
  where
    showLevel [] = "Base"
    showLevel xs = concatMap show xs
    subMods = map WithPlus (subsets mods)
    states = concatMap toList (view _shiftlevels key)
    (xkbLevels, ignoredMods) = unzip (mapMaybe (getLevel key) subMods)

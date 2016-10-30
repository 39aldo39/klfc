{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Xkb.Types where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import qualified Data.Set.Unicode as S
import Util (subsets, (>$>))
import WithPlus (WithPlus(..))
import qualified WithPlus as WP (fromList, singleton)

import Control.Monad.Reader (ReaderT, lift, mapReaderT)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Layout.Key (letterToChar)
import Layout.Layout (getLevel)
import qualified Layout.Modifier as M
import Layout.Types
import Lens.Micro.Platform (view)
import Lookup.Linux (presetTypes, defaultTypes, modifierAndLevelstring)
import Xkb.General (XkbConfig, prepareLayout, supportedModifier)

isRightGuess ∷ Key → Bool
isRightGuess key = rightGuessMods ∧ rightCapslock
  where
    rightGuessMods = view _shiftstates key ≡ guessMods
    rightCapslock = maybe True (≡ view _capslock key) guessAlph
    (guessMods, guessAlph) =
      case fromMaybe (Left 0) (M.lookup (view _pos key) defaultTypes) of
        Left size →
            if size > length (view _letters key)
                then (repeat (∅), Nothing)
                else guessType (view _letters key)
        Right x → x

guessType ∷ [Letter] → ([Shiftstate], Maybe Bool)
guessType []         = ([], Nothing)
guessType [_]        = ([(∅)], Nothing)
guessType [l,l']     = ([(∅), WP.singleton M.Shift], Just (isCapslock l l'))
guessType [l,l',_]   = ([(∅), WP.singleton M.Shift, WP.singleton M.AltGr], Just (isCapslock l l'))
guessType [l,l',_,_] = ([(∅), WP.singleton M.Shift, WP.singleton M.AltGr, WP.fromList [M.Shift,M.AltGr]], Just (isCapslock l l'))
guessType _    = (repeat (∅), Nothing)

-- http://www.charvolant.org/~doug/xkb/html/node5.html#SECTION00054000000000000000
isCapslock ∷ Letter → Letter → Bool
isCapslock l1 l2 = maybe False isLower (letterToChar l1)
                 ∧ maybe False isUpper (letterToChar l2)

keytypeName ∷ Key → String
keytypeName = (\x → fromMaybe x (lookup x presetTypes)) ∘ keytypeName'

keytypeName' ∷ Key → String
keytypeName' key =
    (intercalate "_" ∘ map modsToName ∘ filter (all (∈ map fst modifierAndLevelstring)) ∘ map toList $ view _shiftstates key) ⊕
    bool "" "_ALPHABETIC" (view _capslock key)
  where
    modsToName [] = "NONE"
    modsToName ms = intercalate "+" ∘ map (map toUpper ∘ show) $ ms

printTypes ∷ Layout → ReaderT XkbConfig Logger String
printTypes = mapReaderT (pure ∘ runIdentity) ∘ prepareLayout >=> \layout → do
    types ← lift $ getTypes layout
    let virtualMods = S.unions (map __typeMods types)
    pure $ unlines $
        [ "default xkb_types \"basic\" {"
        ] ⧺ printVirtualMods (filter isVirtualModifier (S.toList virtualMods)) ⧺
        concatMap printType types ⧺
        [ "};"
        , ""
        ]
  where
    isVirtualModifier = (∈ [M.AltGr, M.Extend, M.Win, M.NumLock])
    printVirtualMods [] = [""]
    printVirtualMods xs =
        [ ""
        , "    virtual_modifiers " ⊕ intercalate "," (mapMaybe (`lookup` modifierAndLevelstring) xs) ⊕ ";"
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
    listMods ms = Just ∘ intercalate "+" ∘ mapMaybe (`lookup` modifierAndLevelstring) $ ms

getTypes ∷ Layout → Logger [Type]
getTypes =
    traverse getType ∘ view _keys >$>
    filter (not ∘ isPresetType ∘ __typeName) >>>
    nub
  where
    isPresetType = (∈ map snd presetTypes)

getType ∷ Key → Logger Type
getType key = getType' key <$> filterMS supportedModifier mods
  where
    mods       = (S.∪ extraMods) ∘ S.unions ∘ map getSet ∘ view _shiftstates $ key
    extraMods  = S.fromList [M.CapsLock | view _capslock key]
    filterMS f = fmap S.fromAscList ∘ filterM f ∘ S.toAscList

getType' ∷ Key → Set Modifier → Type
getType' key mods = Type
    { __typeName   = keytypeName key
    , __typeMods   = mods
    , __maps       = zip subMods levels
    , __preserves  = filter (not ∘ null ∘ snd) (zip subMods ignoredMods)
    , __levelNames = zip [0..] (map (showLevel ∘ toList) (view _shiftstates key))
    }
  where
    showLevel [] = "Base"
    showLevel xs = concatMap show xs
    subMods = map WithPlus (subsets mods)
    (levels, ignoredMods) = unzip (mapMaybe (getLevel key) subMods)

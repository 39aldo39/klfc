{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Xkb.Symbols where

import BasePrelude
import Prelude.Unicode
import Data.List.Unicode ((∖))
import Data.Monoid.Unicode ((∅), (⊕))
import Util (show', lookup', groupSortWith, concatMapM, tellMaybeT, (>$>))
import WithPlus (WithPlus(..))
import qualified WithPlus as WP (fromList)

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (State, state, evalState, get, modify)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer (tell, runWriter)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as S
import Lens.Micro.Platform (view, over, _1, _2, makeLenses)

import Layout.Layout (singletonKeyToKey, getPosByLetterAndShiftstate, getLetterByPosAndShiftstate)
import qualified Layout.Modifier as M
import Layout.Types
import Lookup.Linux
import PresetLayout (defaultLayout, defaultFullLayout)
import Xkb.General (XkbConfig(..), prepareLayout)
import Xkb.Types (keytypeName, isRightGuess)

type Group = Int
type TypeName = String
type RightGuess = Bool
type Symbol = String
type XkbAction = String
type VirtualMod = String
data XkbGroup = XkbGroup
    { __xkbGroupTypeName ∷ TypeName
    , __rightGuess ∷ RightGuess
    , __symbols ∷ [Symbol]
    , __actions ∷ [XkbAction]
    , __virtualMods ∷ [VirtualMod]
    }
data XkbKey = XkbKey
    { __xkbPos ∷ String
    , __xkbSymbols ∷ Map Group XkbGroup
    }
makeLenses ''XkbKey

printSymbolsHelp ∷ [Symbol] → String
printSymbolsHelp [] = "[]"
printSymbolsHelp xs = ("[ "⊕) ∘ (⊕" ]") ∘ intercalate ", " ∘ map (printf "%12s") $ xs

printActions ∷ [XkbAction] → String
printActions = ("[ " ⊕) ∘ (⊕ " ]") ∘ intercalate ", " ∘ dropWhileEnd (≡"NoAction()")

printGroup ∷ Group → XkbGroup → State Group [String]
printGroup groupNr (XkbGroup _ _ symbols actions vmods) =
    (: catMaybes [actions', vmods']) <$> symbols'
  where
    symbols' = do
        expGroupNr ← state (id &&& succ)
        case groupNr `compare` expGroupNr of
          LT → pure $ "symbols[Group" ⊕ show groupNr ⊕ "] = " ⊕ printSymbolsHelp symbols
          EQ → pure $ printSymbolsHelp symbols
          GT → ("[], " ⊕) <$> symbols'
    actions'
      | all (≡ "NoAction()") actions = Nothing
      | otherwise = Just ("actions[Group" ⊕ show groupNr ⊕ "] = " ⊕ printActions actions)
    vmods'
      | null vmods = Nothing
      | otherwise  = Just ∘ ("vmods=" ⊕) ∘ intercalate "," $ vmods

printGroups ∷ [(Group, XkbGroup)] → State Group [String]
printGroups = concatMapM (uncurry printGroup)

printXkbKey ∷ XkbKey → [String]
printXkbKey (XkbKey pos syms) = lines $
    "key " ⊕ pos ⊕ " { " ⊕ intercalate (",\n" ⊕ replicate 13 ' ') syms' ⊕ " };"
    where syms' = evalState (printGroups (Map.toAscList syms)) 1

printXkbKeys ∷ [XkbKey] → [String]
printXkbKeys = flip (evalState ∘ stateXkbKeys) (∅)

stateXkbKeys ∷ [XkbKey] → State (Map Group String) [String]
stateXkbKeys = concatMapM $
    liftA2 (⧺)
        <$> concatMapM (uncurry stateXkbKey) ∘ Map.toAscList ∘ __xkbSymbols
        <*> pure ∘ printXkbKey

stateXkbKey ∷ Group → XkbGroup → State (Map Group String) [String]
stateXkbKey groupNr (XkbGroup typeName rightGuess _ _ _) = do
    prev ← Map.lookup groupNr <$> get
    if maybe rightGuess (≡typeName) prev
      then pure []
      else ["key.type[Group" ⊕ show groupNr ⊕ "] = \"" ⊕ typeName ⊕ "\";"] <$
           modify (Map.insert groupNr typeName)

type ExtraKey = String
data XkbData = XkbData
    { __xkbKeys ∷ [XkbKey]
    , __xkbExtraKeys ∷ [ExtraKey]
    }
instance Monoid XkbData where
    mempty = XkbData [] []
    XkbData xs xs' `mappend` XkbData ys ys' =
        XkbData (xs `addXkbKeys` ys) (xs' ⧺ ys')

addXkbKeys ∷ [XkbKey] → [XkbKey] → [XkbKey]
addXkbKeys []           keys2 = keys2
addXkbKeys (key1:keys1) keys2 = key1' : addXkbKeys keys1 keys2'
  where
    (samePos, keys2') = partition (on (≡) __xkbPos key1) keys2
    key1' = over _xkbSymbols (⊕ extraXkbSymbols) key1
    extraXkbSymbols = mconcat (map __xkbSymbols samePos)

printXkbData ∷ XkbData → String
printXkbData (XkbData keys extraKeys) = unlines $
    [ "default partial"
    , "xkb_symbols \"basic\" {"
    ] ⧺ map (replicate 4 ' ' ⊕)
    ( printXkbKeys keys ⧺
      extraKeys
    ) ⧺
    [ "};"
    ]

type RedirectAllXkb = Bool
type RedirectIfExtend = Bool

printSymbols ∷ (Logger m, MonadReader XkbConfig m) ⇒ Layout → m String
printSymbols =
    prepareLayout >=>
    printLayout 1 >$>
    printXkbData

printLayouts ∷ (Logger m, MonadReader XkbConfig m) ⇒ [Layout] → m XkbData
printLayouts = fmap mconcat ∘ zipWithM printLayout [1..]

printLayout ∷ (Logger m, MonadReader XkbConfig m) ⇒ Group → Layout → m XkbData
printLayout groupNr layout =
    XkbData
        <$> (catMaybes <$> traverse (printKey groupNr layout) keys)
        <*> pure extraKeys
  where
    keys = view _keys layout ⧺ map singletonKeyToKey singletonKeys
    (singletonKeys, extraKeys) = printExtraKeys layout

printKey ∷ (Logger m, MonadReader XkbConfig m) ⇒ Group → Layout → Key → m (Maybe XkbKey)
printKey groupNr layout key = runMaybeT $ do
    p ← maybe unsupportedPos pure (lookup pos posAndKeycode)
    ls ← traverse printLetter letters
    actions ← zipWithM (printAction layout pos) levels letters
    let vmods = nub (concatMap printVirtualMods letters)
    let xkbGroup = XkbGroup (keytypeName key) (isRightGuess key) ls actions vmods
    pure $ XkbKey p (Map.singleton groupNr xkbGroup)
  where
    pos = view _pos key
    levels = view _shiftlevels key
    letters = view _letters key
    unsupportedPos = tellMaybeT [show' pos ⊕ " is not supported in XKB"]

printLetter ∷ Logger m ⇒ Letter → m String
printLetter (Char c) =
    pure $ fromMaybe (printf "U%04X" c) (lookup c charAndString)
printLetter (Ligature (Just c) _) =
    printLetter (Char c)
printLetter l@(Ligature Nothing _) =
    "VoidSymbol" <$ tell [show' l ⊕ " has no base character in XKB"]
printLetter (Action a) =
    maybe e (pure ∘ __symbol) (lookup a actionAndLinuxAction)
    where e = "VoidSymbol" <$ tell [show' a ⊕ " is not supported in XKB"]
printLetter (Modifiers effect mods) = pure ∘ fromMaybe "VoidSymbol" ∘ listToMaybe $
    mapMaybe ((`lookup` modifierAndSymbol) ∘ (,) effect) mods
printLetter (Dead d) =
    maybe e pure (lookup d deadKeysAndLinuxDeadKeys)
    where e = "VoidSymbol" <$ tell [show' d ⊕ " is not supported in XKB"]
printLetter (CustomDead _ (DeadKey _ (Just c) _)) =
    printLetter (Char c)
printLetter l@(CustomDead _ (DeadKey _ Nothing _)) =
    "VoidSymbol" <$ tell [show' l ⊕ " has no base character in XKB"]
printLetter (Redirect mods pos) =
    printLetter (redirectToLetter mods pos)
printLetter LNothing =
    pure "VoidSymbol"

printAction ∷ (Logger m, MonadReader XkbConfig m) ⇒ Layout → Pos → Shiftlevel → Letter → m String
printAction layout pos level letter = printAction' letter layout pos level letter

printAction' ∷ (Logger m, MonadReader XkbConfig m) ⇒ Letter → Layout → Pos → Shiftlevel → Letter → m String
printAction' errorL layout pos level (Action a) = fromMaybe e $
    printAction' errorL layout pos level <$> lookup a actionAndRedirect <|>
    printLinuxAction <$> lookup a actionAndLinuxAction
  where
    e = "NoAction()" <$ tell [show' a ⊕ " is not supported in XKB"]
printAction' errorL layout pos level (Redirect rMods rPos) = do
    extraClearedMods ← bool [M.Extend] [] <$> asks __redirectClearsExtend
    let addMods   = rMods ∖ modsIntersection
    let clearMods = modsUnion ∖ (rMods ⧺ extraClearedMods)
    let emptyMods = null addMods ∧ null clearMods

    let linuxActionAt p = XkbRedirect symbol p addMods clearMods
    let printWithAt e = maybe e (printLinuxAction ∘ linuxActionAt) ∘ listToMaybe

    redirectAll ← asks __redirectAllXkb
    case (emptyMods, redirectAll) of
      (False, False) → printWithAt noRedPos (lPosses ⧺ qPosses)
      (False, True ) → printWithAt (noOrigRedPos *> printWithAt noRedPos lPosses) qPosses
      (True , False) → pure "NoAction()"
      (True , True ) → printWithAt noOrigRedPos qPosses
  where
    modsUnion = toList ∘ S.unions ∘ map getSet ∘ toList $ level
    modsIntersection = foldl1 intersect (fmap toList level)
    rShiftstate = WP.fromList rMods
    letter = redirectToLetter rMods rPos
    symbol = fst ∘ runWriter $ printLetter letter
    lPosses = getPosByLetterAndShiftstate letter rShiftstate layout
    qPosses = filter sameSymbol (getPosByLetterAndShiftstate letter rShiftstate defaultLayout)
    sameSymbol p = maybe True ((≡ symbol) ∘ fst ∘ runWriter ∘ printLetter) (getLetterByPosAndShiftstate p rShiftstate layout)
    noOrigRedPos = "NoAction()" <$ tell [show' errorL ⊕ " on " ⊕ show' pos ⊕ " could not redirect to the original position in XKB"]
    noRedPos = "NoAction()" <$ tell [show' errorL ⊕ " on " ⊕ show' pos ⊕ " could not find a redirect position in XKB"]
printAction' _ _ _ _ l@(Modifiers effect mods) =
    printLinuxAction (effectToAction effect symbol mods)
  where
    symbol = fst ∘ runWriter $ printLetter l
    effectToAction Shift = SetMods
    effectToAction Latch = LatchMods
    effectToAction Lock  = LockMods
printAction' _ _ _ _ _ = pure "NoAction()"

printVirtualMods ∷ Letter → [String]
printVirtualMods (Modifiers _ mods) =
    mapMaybe (`lookup` modifierAndPressedModifier) (filter (∈ virtualMods) mods)
printVirtualMods _ = []

redirectToLetter ∷ [Modifier] → Pos → Letter
redirectToLetter mods pos = fromMaybe e $
    getLetterByPosAndShiftstate pos (WP.fromList mods) defaultFullLayout
    where e = error ("redirecting to " ⊕ show' pos ⊕ " is not supported")

printExtraKeys ∷ Layout → ([SingletonKey], [String])
printExtraKeys layout = (singletonKeys, modMappings ⧺ replacedModMappings ⧺ includes)
  where
    (singletonKeys, includes) = singletonKeysToIncludes (view _singletonKeys layout)

    modMappings = ($ view (_keys ∘ traverse ∘ _letters) layout ⧺ map (view _sLetter) singletonKeys) $
        map (fst ∘ runWriter ∘ printLetter) >>>
        mapMaybe (\symbol → find ((≡ symbol) ∘ snd) extraModifierMaps) >>>
        groupSortWith fst >>>
        map (uncurry printModMap ∘ over (_2 ∘ traverse) snd)

    replacedModMappings = ($ view _keys layout) $
        mapMaybe (\key → (,) (view _pos key) <$> listToMaybe (view _letters key)) >>>
        (⧺ map (view _sPos &&& view _sLetter) singletonKeys) >>>
        over (traverse ∘ _2) (fst ∘ runWriter ∘ printLetter) >>>
        mapMaybe (uncurry getReplacedModMapping) >>>
        groupSortWith fst >>>
        map (uncurry printModMap ∘ over (_2 ∘ traverse) snd)

    getReplacedModMapping pos symbol = do
        (realMod, origPos) ← lookup symbol defaultModifierMaps
        guard (origPos ∉ map (view _pos) (view _keys layout) ⧺ map (view _sPos) (view _singletonKeys layout))
        let smallerScancode = liftA2 (<) `on` flip lookup posAndScancode
        guard =<< pos `smallerScancode` origPos
        (,) realMod <$> lookup origPos posAndKeycode

    printModMap realMod mods =
        "modifier_map " ⊕ realMod ⊕ " { " ⊕ intercalate ", " mods ⊕ " };"

singletonKeysToIncludes ∷ [SingletonKey] → ([SingletonKey], [String])
singletonKeysToIncludes [] = ([], [])
singletonKeysToIncludes (x:xs) = fromMaybe none (double <|> single)
  where
    double = listToMaybe $ do
        x' ← xs
        s ← lookup' (x, x') doubleIncludes <|>
            lookup' (x', x) doubleIncludes
        pure $ over _2 (s:) (singletonKeysToIncludes (delete x' xs))
    single = do
        s ← lookup x singleIncludes
        pure $ over _2 (s:) (singletonKeysToIncludes xs)
    none = over _1 (x:) (singletonKeysToIncludes xs)

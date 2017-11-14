{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module Xkb.Symbols where

import BasePrelude
import Prelude.Unicode
import Data.List.Unicode ((∖))
import Data.Monoid.Unicode ((∅), (⊕))
import Util (show', lookup', groupSortWith, concatMapM, tellMaybeT, versionStr)
import WithPlus (WithPlus(..))
import qualified WithPlus as WP (fromList)

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (State, state, evalState, get, modify)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer (tell, runWriter)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as S
import Lens.Micro.Platform (view, set, over, _1, _2, makeLenses)

import Layout.Layout (singletonKeyToKey, getLetterByPosAndShiftstate, getPosByEqAndShiftstate, variantToLayout)
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
makeLenses ''XkbGroup
data XkbKey = XkbKey
    { __xkbPos ∷ String
    , __xkbGroups ∷ Map Group XkbGroup
    }
makeLenses ''XkbKey

printSymbolsHelp ∷ [Symbol] → String
printSymbolsHelp [] = "[]"
printSymbolsHelp xs = ("[ "⊕) ∘ (⊕" ]") ∘ intercalate ", " ∘ map (printf "%12s") ∘ dropWhileEnd (≡ "NoSymbol") $ xs

printActions ∷ [XkbAction] → String
printActions = ("[ " ⊕) ∘ (⊕ " ]") ∘ intercalate ", " ∘ dropWhileEnd (≡ "NoAction()")

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

printXkbKeys ∷ [XkbKey] → [XkbKey] → [String]
printXkbKeys baseKeys = flip (evalState ∘ stateXkbKeys baseKeys) (∅)

stateXkbKeys ∷ [XkbKey] → [XkbKey] → State (Map Group String) [String]
stateXkbKeys baseKeys = concatMapM $
    liftA2 adaptKeyToBaseKey getBaseKey id >>>
    liftA2 (⧺)
        <$> concatMapM (uncurry stateXkbKey) ∘ Map.toAscList ∘ __xkbGroups
        <*> pure ∘ printXkbKey
  where
    getBaseKey key = listToMaybe (filter (((≡) `on` __xkbPos) key) baseKeys)
    adaptKeyToBaseKey baseKey = over _xkbGroups (Map.mapWithKey (modifyXkbGroup baseKey))
    modifyXkbGroup baseKey groupNr
      | Just baseXkbGroup ← baseKey >>= Map.lookup groupNr ∘ __xkbGroups
      = set _rightGuess <$> ((≡) `on` __xkbGroupTypeName) baseXkbGroup <*> id >>>
        over _symbols (zipWith (\baseSymbol symbol → bool symbol "NoSymbol"   (baseSymbol ≡ symbol)) (__symbols baseXkbGroup ⧺ repeat "NoSymbol")) >>>
        over _actions (zipWith (\baseAction action → bool action "NoAction()" (baseAction ≡ action)) (__actions baseXkbGroup ⧺ repeat "NoAction()"))
      | otherwise = id

stateXkbKey ∷ Group → XkbGroup → State (Map Group String) [String]
stateXkbKey groupNr (XkbGroup typeName rightGuess _ _ _) = do
    prev ← Map.lookup groupNr <$> get
    if maybe rightGuess (≡typeName) prev
      then pure []
      else ["key.type[Group" ⊕ show groupNr ⊕ "] = \"" ⊕ typeName ⊕ "\";"] <$
           modify (Map.insert groupNr typeName)

type ExtraKey = String
data XkbData = XkbData
    { __xkbLayoutName ∷ String
    , __xkbName ∷ String
    , __xkbKeys ∷ [XkbKey]
    , __xkbExtraKeys ∷ [ExtraKey]
    }
instance Monoid XkbData where
    mempty = XkbData [] [] [] []
    XkbData name xs xs' xs'' `mappend` XkbData _ ys ys' ys'' =
        XkbData name (xs ⧺ ys) (xs' `addXkbKeys` ys') (xs'' ⧺ ys'')

addXkbKeys ∷ [XkbKey] → [XkbKey] → [XkbKey]
addXkbKeys []           keys2 = keys2
addXkbKeys (key1:keys1) keys2 = key1' : addXkbKeys keys1 keys2'
  where
    (samePos, keys2') = partition (on (≡) __xkbPos key1) keys2
    key1' = over _xkbGroups (⊕ extraXkbSymbols) key1
    extraXkbSymbols = mconcat (map __xkbGroups samePos)

printXkbDatas ∷ XkbData → [XkbData] → String
printXkbDatas xkbData variantXkbDatas = unlines $
    [ "// Generated by KLFC " ⊕ versionStr
    , "// https://github.com/39aldo39/klfc"
    ] ⧺ printXkbData (∅) xkbData
    ⧺ concatMap (printXkbData xkbData) variantXkbDatas

printXkbData ∷ XkbData → XkbData → [String]
printXkbData (XkbData _ _ baseKeys _) (XkbData layoutName name keys extraKeys) =
    [ "" ] ⧺
    [ "default" | name ≡ "basic" ] ⧺
    [ "xkb_symbols " ⊕ show name ⊕ " {"
    ] ⧺ map (replicate 4 ' ' ⊕)
    ( [ "include " ⊕ show (layoutName ⊕ "(basic)") | name ≢ "basic" ] ⧺
      printXkbKeys baseKeys keys ⧺
      extraKeys
    ) ⧺
    [ "};"
    ]

type RedirectAllXkb = Bool
type RedirectIfExtend = Bool

printSymbols ∷ (Logger m, MonadReader XkbConfig m) ⇒ Layout → m String
printSymbols = prepareLayout >=> \layout → do
    let layoutName = view (_info ∘ _name) layout
    xkbData ← printLayout layoutName 1 (∅) (Variant (set (_info ∘ _name) "basic" layout))
    variantXkbDatas ← traverse (printLayout layoutName 1 layout) (view _variants layout)
    pure (printXkbDatas xkbData variantXkbDatas)

printLayout ∷ (Logger m, MonadReader XkbConfig m) ⇒ String → Group → Layout → Variant → m XkbData
printLayout layoutName groupNr baseLayout variant =
    printLayout' layoutName groupNr fullLayout variantLayout'
  where
    variantLayout = variantToLayout variant
    fullLayout = baseLayout ⊕ variantLayout
    variantLayout' = ($ fullLayout) $
            over _keys (filter ((∈ posses) ∘ view _pos)) >>>
            over _singletonKeys (filter ((∈ sPosses) ∘ view _sPos)) >>>
            set (_info ∘ _name) (view (_info ∘ _name) variantLayout)
    posses = map (view _pos) (view _keys variantLayout)
    sPosses = map (view _sPos) (view _singletonKeys variantLayout)

printLayout' ∷ (Logger m, MonadReader XkbConfig m) ⇒ String → Group → Layout → Layout → m XkbData
printLayout' layoutName groupNr fullLayout variantLayout =
    XkbData layoutName (view (_info ∘ _name) variantLayout)
        <$> (catMaybes <$> traverse (printKey groupNr fullLayout) keys)
        <*> pure extraKeys
  where
    keys = view _keys variantLayout ⧺ map singletonKeyToKey singletonKeys
    (singletonKeys, extraKeys) = printExtraKeys variantLayout

printKey ∷ (Logger m, MonadReader XkbConfig m) ⇒ Group → Layout → Key → m (Maybe XkbKey)
printKey groupNr fullLayout key = runMaybeT $ do
    p ← maybe unsupportedPos pure (lookup pos posAndKeycode)
    ls ← traverse printLetter letters
    actions ← zipWithM (printAction fullLayout pos) levels letters
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
printAction fullLayout pos level letter = printAction' letter fullLayout pos level letter

printAction' ∷ (Logger m, MonadReader XkbConfig m) ⇒ Letter → Layout → Pos → Shiftlevel → Letter → m String
printAction' errorL fullLayout pos level (Action a) = fromMaybe e $
    printAction' errorL fullLayout pos level <$> lookup a actionAndRedirect <|>
    printLinuxAction <$> lookup a actionAndLinuxAction
  where
    e = "NoAction()" <$ tell [show' a ⊕ " is not supported in XKB"]
printAction' errorL fullLayout pos level (Redirect rMods rPos) = do
    extraClearedMods ← bool [M.Extend] [] <$> asks __redirectClearsExtend
    let addMods   = rMods ∖ modsIntersection
    let clearMods = modsUnion ∖ (rMods ⧺ extraClearedMods)
    let emptyMods = null addMods ∧ null clearMods

    let rShiftstate = WP.fromList (rMods ⧺ extraClearedMods)
    let sameSymbol = (≡ symbol) ∘ fst ∘ runWriter ∘ printLetter
    let sameSymbolPos p = maybe True sameSymbol (getLetterByPosAndShiftstate p rShiftstate fullLayout)
    let lPosses = getPosByEqAndShiftstate sameSymbol rShiftstate fullLayout
    let qPosses = filter sameSymbolPos (getPosByEqAndShiftstate sameSymbol rShiftstate defaultLayout)

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
    letter = redirectToLetter rMods rPos
    symbol = fst ∘ runWriter $ printLetter letter
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

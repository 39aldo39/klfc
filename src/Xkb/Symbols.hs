{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Xkb.Symbols where

import BasePrelude
import Prelude.Unicode
import Data.List.Unicode ((∖))
import Data.Monoid.Unicode ((∅), (⊕))
import Util (show', lookup', removeSubList, concatMapM, tellMaybeT, lift2, (>$>))
import WithPlus (WithPlus(..))
import qualified WithPlus as WP (fromList)

import Control.Monad.Reader (ReaderT, lift, asks, mapReaderT)
import Control.Monad.State (State, state, evalState, get, modify)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer (tell, runWriter, mapWriter)
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import qualified Data.Map as M
import Lens.Micro.Platform (view, over, _1, _2, makeLenses)

import Layout.Layout (getPosByLetterAndShiftstate, getLetterByPosAndShiftstate)
import qualified Layout.Modifier as M
import Layout.Types
import Lookup.Linux
import PresetLayout (defaultLayout, defaultFullLayout)
import Xkb.General (XkbConfig(..), prepareLayout, supportedShiftstate)
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
    , __virtualMods ∷ Maybe [VirtualMod]
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
    vmods' = ("vmods=" ⊕) ∘ intercalate "," <$> vmods

printGroups ∷ [(Group, XkbGroup)] → State Group [String]
printGroups = concatMapM (uncurry printGroup)

printXkbKey ∷ XkbKey → [String]
printXkbKey (XkbKey pos syms) = lines $
    "key " ⊕ pos ⊕ " { " ⊕ intercalate (",\n" ⊕ replicate 13 ' ') syms' ⊕ " };"
    where syms' = evalState (printGroups (M.toAscList syms)) 1

printXkbKeys ∷ [XkbKey] → [String]
printXkbKeys = flip (evalState ∘ stateXkbKeys) (∅)

stateXkbKeys ∷ [XkbKey] → State (Map Group String) [String]
stateXkbKeys = concatMapM $
    liftA2 (⧺)
        <$> concatMapM (uncurry stateXkbKey) ∘ M.toAscList ∘ __xkbSymbols
        <*> pure ∘ printXkbKey

stateXkbKey ∷ Group → XkbGroup → State (Map Group String) [String]
stateXkbKey groupNr (XkbGroup typeName rightGuess _ _ _) = do
    prev ← M.lookup groupNr <$> get
    if maybe rightGuess (≡typeName) prev
      then pure []
      else ["key.type[Group" ⊕ show groupNr ⊕ "] = \"" ⊕ typeName ⊕ "\";"] <$
           modify (M.insert groupNr typeName)

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

printSymbols ∷ Layout → ReaderT XkbConfig Logger String
printSymbols =
    mapReaderT (pure ∘ runIdentity) ∘ prepareLayout >=>
    printLayout 1 >$>
    printXkbData

printLayouts ∷ [Layout] → ReaderT XkbConfig Logger XkbData
printLayouts = fmap mconcat ∘ zipWithM printLayout [1..]

printLayout ∷ Group → Layout → ReaderT XkbConfig Logger XkbData
printLayout groupNr =
    liftA2 XkbData
        <$> fmap catMaybes ∘ liftA2 traverse (printKey groupNr) (view _keys)
        <*> lift ∘ printSingletonKeys ∘ view _singletonKeys

printKey ∷ Group → Layout → Key → ReaderT XkbConfig Logger (Maybe XkbKey)
printKey groupNr layout key = mapReaderT runMaybeT $ do
    p ← maybe unsupportedPos pure (lookup pos posAndKeycode)
    statesAndLetters ← lift2 $ filterM (supportedShiftstate ∘ fst) (states `zip` letters)
    ls ← lift2 $ traverse (printLetter ∘ snd) statesAndLetters
    actions ← mapReaderT lift $ traverse (uncurry (printAction layout pos)) statesAndLetters
    let vmods = nub ∘ concat <$> traverse printVirtualMods letters
    let xkbGroup = XkbGroup (keytypeName key) (isRightGuess key) ls actions vmods
    pure $ XkbKey p (M.singleton groupNr xkbGroup)
  where
    pos = view _pos key
    states = view _shiftstates key
    letters = view _letters key
    unsupportedPos = lift $ tellMaybeT [show' pos ⊕ " is not supported in XKB"]

printLetter ∷ Letter → Logger String
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

printAction ∷ Layout → Pos → Shiftstate → Letter → ReaderT XkbConfig Logger String
printAction layout pos shiftstate (Action a) = fromMaybe e $
    removeRed ∘ printAction layout pos shiftstate <$> lookup a actionAndRedirect <|>
    lift ∘ printLinuxAction <$> lookup a actionAndLinuxAction
  where
    removeRed = mapReaderT (mapWriter (over _2 (map (removeSubList "red:"))))
    e = "NoAction()" <$ tell [show' a ⊕ " is not supported in XKB"]
printAction layout pos shiftstate l@(Redirect rMods rPos) = do
    extraClearedMods ← bool [M.Extend] [] <$> asks __redirectClearsExtend
    let addMods   = rMods ∖ mods
    let clearMods = mods ∖ (rMods ⧺ extraClearedMods)
    let emptyMods = null addMods ∧ null clearMods

    let linuxActionAt p = XkbRedirect symbol p addMods clearMods
    let printWithAt e = maybe e (lift ∘ printLinuxAction ∘ linuxActionAt) ∘ listToMaybe

    redirectAll ← asks __redirectAllXkb
    case (emptyMods, redirectAll) of
      (False, False) → printWithAt noRedPos (lPosses ⧺ qPosses)
      (False, True ) → printWithAt (noOrigRedPos *> printWithAt noRedPos lPosses) qPosses
      (True , False) → pure "NoAction()"
      (True , True ) → printWithAt noOrigRedPos qPosses
  where
    mods = toList (getSet shiftstate)
    rShiftstate = WP.fromList rMods
    letter = redirectToLetter rMods rPos
    symbol = fst ∘ runWriter $ printLetter letter
    lPosses = getPosByLetterAndShiftstate letter rShiftstate layout
    qPosses = filter sameSymbol (getPosByLetterAndShiftstate letter rShiftstate defaultLayout)
    sameSymbol p = maybe True ((≡ symbol) ∘ fst ∘ runWriter ∘ printLetter) (getLetterByPosAndShiftstate p rShiftstate layout)
    noOrigRedPos = lift $ "NoAction()" <$ tell [show' l ⊕ " on " ⊕ show' pos ⊕ " could not redirect to the original position in XKB"]
    noRedPos = lift $ "NoAction()" <$ tell [show' l ⊕ " on " ⊕ show' pos ⊕ " could not find a redirect position in XKB"]
printAction _ _ _ l@(Modifiers effect mods) = lift $
    printLinuxAction (effectToAction effect symbol mods)
  where
    symbol = fst ∘ runWriter $ printLetter l
    effectToAction Shift = SetMods
    effectToAction Latch = LatchMods
    effectToAction Lock  = LockMods
printAction _ _ _ _ = pure "NoAction()"

printVirtualMods ∷ Letter → Maybe [String]
printVirtualMods (Modifiers _ mods) = Just $
    mapMaybe (`lookup` modifierAndPressedModifier) (filter (∈ virtualMods) mods)
printVirtualMods _ = Nothing

redirectToLetter ∷ [Modifier] → Pos → Letter
redirectToLetter mods pos = fromMaybe e $
    getLetterByPosAndShiftstate pos (WP.fromList mods) defaultFullLayout
    where e = error ("redirecting to " ⊕ show' pos ⊕ " is not supported")

printSingletonKeys ∷ [SingletonKey] → Logger [String]
printSingletonKeys startSingletonKeys =
  case over _2 addEmpty (singletonKeysToIncludes startSingletonKeys) of
    ([], includes) → pure includes
    (singletonKeys, includes) → do
        extraKeys ← catMaybes <$> traverse (uncurry printSingletonKey) singletonKeys
        let modMappings = mapMaybe (flip lookup modMappingIncludes ∘ snd) singletonKeys
        pure $
            "":
            "key.type[group1] = \"ONE_LEVEL\";":
            extraKeys ⧺
            modMappings ⧺
            includes
  where
    addEmpty [] = []
    addEmpty xs = []:xs

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

printSingletonKey ∷ Pos → Letter → Logger (Maybe String)
printSingletonKey pos letter = runMaybeT $ do
    p ← maybe e pure (lookup pos posAndKeycode)
    l ← lift $ printLetter letter
    pure $ "key " ⊕ p ⊕ " { [ " ⊕ l ⊕ " ] };"
  where
    e = tellMaybeT [show' pos ⊕ " is not supported in XKB"]

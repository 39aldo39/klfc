{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Xkb.General where

import BasePrelude
import Prelude.Unicode hiding ((∈))
import Data.Foldable.Unicode ((∈))
import Data.Monoid.Unicode ((∅), (⊕))
import Util (show', (>$>), privateChars)
import qualified WithPlus as WP (singleton)

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState, evalState)
import Control.Monad.Writer (tell)
import Lens.Micro.Platform (view, over)

import Layout.Key (setNullChar, filterKeyOnShiftstatesM)
import Layout.Layout (getLetterByPosAndShiftstate, addDefaultKeys)
import Lookup.Linux (modifierAndTypeModifier, modifierAndPressedModifier)
import qualified Layout.Modifier as M
import Layout.Types
import PresetLayout (defaultKeys, defaultFullLayout)

data XkbConfig = XkbConfig
    { __addShortcuts ∷ Bool
    , __redirectAllXkb ∷ Bool
    , __redirectClearsExtend ∷ Bool
    }

prepareLayout ∷ (Logger m, MonadReader XkbConfig m) ⇒ Layout → m Layout
prepareLayout layout = do
    addShortcuts ← asks __addShortcuts
    ($ layout) $
        addDefaultKeys defaultKeys >>>
         _keys
            ( traverse (filterKeyOnShiftstatesM supportedShiftstate) >$>
              bool id (map addShortcutLetters) addShortcuts
            ) >$>
        flip evalState privateChars ∘ setNullChars

setNullChars ∷ MonadState [Char] m ⇒ Layout → m Layout
setNullChars =
    (_keys ∘ traverse ∘ _letters ∘ traverse) setNullChar >=>
    (_variants ∘ traverse) (fmap Variant ∘ setNullChars ∘ variantToLayout)

supportedShiftstate ∷ Logger m ⇒ Shiftstate → m Bool
supportedShiftstate = fmap and ∘ traverse supportedTypeModifier ∘ toList

supportedTypeModifier ∷ Logger m ⇒ Modifier → m Bool
supportedTypeModifier modifier
    | modifier ∈ map fst modifierAndTypeModifier = pure True
    | otherwise = False <$ tell [show' modifier ⊕ " is not supported in XKB"]

supportedPressedModifier ∷ Logger m ⇒ Modifier → m Bool
supportedPressedModifier modifier
    | modifier ∈ map fst modifierAndPressedModifier = pure True
    | otherwise = False <$ tell [show' modifier ⊕ " is not supported in XKB"]

addShortcutLetters ∷ Key → Key
addShortcutLetters key | any (WP.singleton M.Control ∈) (view _shiftlevels key) = key
addShortcutLetters key = fromMaybe key $
    over _shiftlevels (M.singleton M.Control :) <$>
    _letters (liftA2 (:) (getLetterByPosAndShiftstate shortcutPos (∅) defaultFullLayout) ∘ pure) key
  where
    shortcutPos = view _shortcutPos key

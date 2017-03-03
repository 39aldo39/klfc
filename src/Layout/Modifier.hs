{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Layout.Modifier
    ( Modifier(..)
    , Shiftstate
    , toBaseModifier
    , toEqualModifiers
    , getEqualModifiers
    , controlMods
    , activatedBy
    ) where

import BasePrelude hiding (Alt, Control)
import Prelude.Unicode hiding ((∈))
import Data.Foldable.Unicode ((∈))
import Util (HumanReadable(..))
import WithPlus (WithPlus)
import qualified WithPlus as WP

import Data.Set (Set)

data Modifier
    = Shift
    | Shift_L
    | Shift_R
    | CapsLock
    | Win
    | Win_L
    | Win_R
    | Alt
    | Alt_L
    | Alt_R
    | Control
    | Control_L
    | Control_R
    | NumLock
    | AltGr
    | Extend
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance HumanReadable Modifier where
    typeName _ = "modifier"
    toString = show

toBaseModifier ∷ Modifier → Modifier
toBaseModifier Shift_L = Shift
toBaseModifier Shift_R = Shift
toBaseModifier Win_L = Win
toBaseModifier Win_R = Win
toBaseModifier Alt_L = Alt
toBaseModifier Alt_R = Alt
toBaseModifier Control_L = Control
toBaseModifier Control_R = Control
toBaseModifier modifier = modifier

toEqualModifiers ∷ Modifier → [Modifier]
toEqualModifiers modifier = nub [toBaseModifier modifier, modifier]

getEqualModifiers ∷ Modifier → [Modifier]
getEqualModifiers Shift = [Shift, Shift_L, Shift_R]
getEqualModifiers Win = [Win, Win_L, Win_R]
getEqualModifiers Alt = [Alt, Alt_L, Alt_R]
getEqualModifiers Control = [Control, Control_L, Control_R]
getEqualModifiers modifier = [modifier]

controlMods ∷ [Modifier]
controlMods = [Win, Win_L, Win_R, Alt, Alt_L, Alt_R, Control, Control_L, Control_R]

type Shiftstate = WithPlus Modifier

instance HumanReadable Shiftstate where
    typeName _ = "shiftstate"
    toString = WP.toString
    parseString = WP.parseString

activatedBy ∷ Set Modifier → Shiftstate → Bool
activatedBy mods state =
    all (any (∈ state) ∘ toEqualModifiers) mods ∧
    all (any (∈ mods) ∘ getEqualModifiers) state

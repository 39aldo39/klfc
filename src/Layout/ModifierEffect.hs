{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Layout.ModifierEffect
    ( ModifierEffect(..)
    , defaultModifierEffect
    ) where

import BasePrelude
import Util (HumanReadable(..))

import Layout.Modifier (Modifier(CapsLock, NumLock))

data ModifierEffect = Shift | Latch | Lock deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance HumanReadable ModifierEffect where
    typeName _ = "modifier effect"
    toString = show

defaultModifierEffect ∷ Modifier → ModifierEffect
defaultModifierEffect CapsLock = Lock
defaultModifierEffect NumLock = Lock
defaultModifierEffect _ = Shift

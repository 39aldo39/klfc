{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Layout.Modifier
    ( Modifier(..)
    , Shiftstate
    , controlMods
    ) where

import BasePrelude hiding (Alt, Control)
import Util (HumanReadable(..))
import WithPlus (WithPlus)

data Modifier
    = Shift
    | CapsLock
    | Win
    | Alt
    | Control
    | NumLock
    | AltGr
    | Extend
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance HumanReadable Modifier where
    typeName _ = "modifier"
    toString = show

controlMods ∷ [Modifier]
controlMods = [Win, Alt, Control]

type Shiftstate = WithPlus Modifier

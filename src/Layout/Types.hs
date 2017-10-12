{-# LANGUAGE ConstraintKinds #-}

module Layout.Types
    ( Action
    , DeadKey(..)
    , ActionMap
    , ActionResult(..)
    , Letter(..)
    , Key
    , _pos
    , _shortcutPos
    , _shiftlevels
    , _letters
    , _capslock
    , Information
    , _fullName
    , _name
    , _copyright
    , _company
    , _localeId
    , _version
    , _description
    , Layout
    , _info
    , _singletonKeys
    , _mods
    , _variants
    , _keys
    , SingletonKey(..)
    , _sPos
    , _sLetter
    , Variant(..)
    , Logger
    , Mod(..)
    , Modifier
    , ModifierEffect(..)
    , Shiftstate
    , Shiftlevel
    , Pos
    ) where

import Layout.Action (Action)
import Layout.DeadKey (DeadKey(..), ActionMap, ActionResult(..))
import Layout.Key (Letter(..), Key, _pos, _shortcutPos, _shiftlevels, _letters, _capslock)
import Layout.Layout (Information(..), _fullName, _name, _copyright, _company, _localeId, _version, _description, Layout(..), _info, _singletonKeys, _mods, _variants, _keys, SingletonKey(..), _sPos, _sLetter, Variant(..))
import Layout.Mod (Mod(..))
import Layout.Modifier (Modifier, Shiftstate, Shiftlevel)
import Layout.ModifierEffect (ModifierEffect(..))
import Layout.Pos (Pos)

import Control.Monad.Writer (MonadWriter)

type Logger = MonadWriter [String]

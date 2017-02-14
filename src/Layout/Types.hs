{-# LANGUAGE ConstraintKinds #-}

module Layout.Types
    ( Action
    , DeadKey(..)
    , StringMap
    , Letter(..)
    , Key
    , _pos
    , _shortcutPos
    , _shiftstates
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
    , _keys
    , SingletonKey
    , Logger
    , Mod(..)
    , Modifier
    , ModifierEffect(..)
    , Shiftstate
    , Pos
    ) where

import Layout.Action (Action)
import Layout.DeadKey (DeadKey(..), StringMap)
import Layout.Key (Letter(..), Key, _pos, _shortcutPos, _shiftstates, _letters, _capslock)
import Layout.Layout (Information(..), _fullName, _name, _copyright, _company, _localeId, _version, _description, Layout(..), _info, _singletonKeys, _mods, _keys, SingletonKey)
import Layout.Mod (Mod(..))
import Layout.Modifier (Modifier, Shiftstate)
import Layout.ModifierEffect (ModifierEffect(..))
import Layout.Pos (Pos)

import Control.Monad.Writer (MonadWriter)

type Logger = MonadWriter [String]

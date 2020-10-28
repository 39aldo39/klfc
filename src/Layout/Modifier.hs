{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}

module Layout.Modifier
    ( Modifier(..)
    , Shiftstate
    , Shiftlevel
    , toBaseModifier
    , toEqualModifiers
    , getEqualModifiers
    , controlMods
    , ExtendId
    , toExtendId
    , fromExtendId
    , isExtend
    , parseJSONShiftlevels
    , empty
    , singleton
    , fromList
    , activatedBy
    ) where

import BasePrelude hiding (Alt, Control, empty, fromList)
import Prelude.Unicode hiding ((∈))
import Data.Foldable.Unicode ((∈))
import Data.Monoid.Unicode ((∅), (⊕))
import Util (HumanReadable(..), lookupByR)
import WithBar (WithBar(..))
import qualified WithBar as WB
import WithPlus (WithPlus)
import qualified WithPlus as WP

import qualified Control.Monad.Fail as Fail (fail)
import Data.Aeson.Types (Parser, Object, parseJSON)
import qualified Data.HashMap.Lazy as HM

data Modifier
    = CapsLock
    | Shift
    | Shift_L
    | Shift_R
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
    | ExtraExtend Int
    deriving (Eq, Ord, Show, Read)

baseStringList ∷ [(Modifier, String)]
baseStringList = map (id &&& show) [CapsLock, Shift, Shift_L, Shift_R, Win, Win_L, Win_R, Alt, Alt_L, Alt_R, Control, Control_L, Control_R, NumLock, AltGr, Extend]

instance HumanReadable Modifier where
    typeName _ = "modifier"
    parseString s
      | Just n ← readMaybe =<< stripPrefix "Extend" s = pure (ExtraExtend n)
      | otherwise = maybe e pure (lookupByR eq s baseStringList)
      where
        eq = (≡) `on` map toLower ∘ filter (≢'_')
        e = Fail.fail ("‘" ⊕ s ⊕ "’ is not a valid modifier")
    toString (ExtraExtend n) = "Extend" ⊕ show n
    toString m = show m

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

type ExtendId = Maybe Int

toExtendId ∷ Modifier → Maybe ExtendId
toExtendId Extend = Just Nothing
toExtendId (ExtraExtend n) = Just (Just n)
toExtendId _ = Nothing

fromExtendId ∷ ExtendId → Modifier
fromExtendId Nothing = Extend
fromExtendId (Just n) = ExtraExtend n

isExtend ∷ Modifier → Bool
isExtend = isJust ∘ toExtendId

type Shiftstate = WithPlus Modifier

instance HumanReadable Shiftstate where
    typeName _ = "shiftstate"
    toString = WP.toString
    parseString = WP.parseString

type Shiftlevel = WithBar Shiftstate

instance HumanReadable Shiftlevel where
    typeName _ = "shiftlevel"
    toString = WB.toString
    parseString = WB.parseString

parseJSONShiftlevels ∷ Object → Maybe (Parser [Shiftlevel])
parseJSONShiftlevels o =
    parseJSON <$> HM.lookup "shiftlevels" o <|>
    fmap (map (WithBar ∘ (:| []))) ∘ parseJSON <$> HM.lookup "shiftstates" o

empty ∷ Shiftlevel
empty = WithBar ((∅) :| [])

singleton ∷ Modifier → Shiftlevel
singleton = WithBar ∘ (:| []) ∘ WP.singleton

fromList ∷ [Modifier] → Shiftlevel
fromList = WithBar ∘ (:| []) ∘ WP.fromList

activatedBy ∷ Foldable t ⇒ t Modifier → Shiftlevel → Bool
activatedBy = any ∘ activatedBy'

activatedBy' ∷ Foldable t ⇒ t Modifier → Shiftstate → Bool
activatedBy' mods state =
    all (any (∈ state) ∘ toEqualModifiers) mods ∧
    all (any (∈ mods) ∘ getEqualModifiers) state

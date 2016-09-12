{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Layout.Mod
    ( Mod(Mod)
    , isEmptyMod
    , applyMod
    ) where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))

import Data.Aeson

import Layout.Pos (Pos)
import Permutation (Permutation, isEmptyPermutation, permutate)

data Mod = Mod
    { name ∷ String
    , permutation ∷ Permutation Pos
    } deriving (Eq, Ord, Show, Read, Generic)

isEmptyMod ∷ Mod → Bool
isEmptyMod (Mod "" p) = isEmptyPermutation p
isEmptyMod _ = False

instance Monoid Mod where
    mempty = Mod (∅) (∅)
    Mod a1 a2 `mappend` Mod b1 b2 = Mod (a1 ⊕ b1) (a2 ⊕ b2)

instance ToJSON Mod
instance FromJSON Mod

applyMod ∷ Mod → Pos → Pos
applyMod = permutate ∘ permutation

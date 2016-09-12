{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WithPlus
    ( WithPlus(..)
    , fromList
    , singleton
    ) where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (HumanReadable(..), split)

import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import Data.Aeson.Types (Value(String, Array), typeMismatch)
import Data.Set (Set)
import qualified Data.Set as S (fromList, singleton)
import qualified Data.Text as T (unpack)

newtype WithPlus α = WithPlus { getSet ∷ Set α }
    deriving (Eq, Ord, Show, Read, Foldable, Monoid)

fromList ∷ Ord α ⇒ [α] → WithPlus α
fromList = WithPlus ∘ S.fromList

singleton ∷ α → WithPlus α
singleton = WithPlus ∘ S.singleton

instance (Ord α, HumanReadable α) ⇒ HumanReadable (WithPlus α) where
    typeName _ = typeName (Proxy ∷ Proxy α) ⊕ "s"
    toString xs
      | null xs = "None"
      | otherwise = (intercalate "+" ∘ map toString ∘ toList) xs
    parseString s
      | null s = pure (∅)
      | map toLower s ≡ "none" = pure (∅)
      | otherwise = fromList <$> traverse parseString (toList (split (≡'+') s))

instance (Ord α, HumanReadable α) ⇒ ToJSON (WithPlus α) where
    toJSON = hrToJSON

instance (Ord α, HumanReadable α) ⇒ FromJSON (WithPlus α) where
    parseJSON (String s) = parseString (T.unpack s)
    parseJSON (Array a) = fromList <$> traverse hrParseJSON (toList a)
    parseJSON v = typeMismatch "String or Array" v

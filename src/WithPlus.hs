{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WithPlus
    ( WithPlus(..)
    , fromList
    , singleton
    , toString
    , parseString
    ) where

import BasePrelude hiding (toList, fromList, singleton)
import Prelude.Unicode
import Data.Monoid.Unicode ((∅))
import Util (HumanReadable, split)
import qualified Util as HR (HumanReadable(..))

import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import Data.Aeson.Types (Value(String, Array), typeMismatch)
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as S (fromList, singleton)
import qualified Data.Text as T (pack, unpack)

newtype WithPlus α = WithPlus { getSet ∷ Set α }
    deriving (Eq, Ord, Show, Read, Foldable, Semigroup, Monoid)

fromList ∷ Ord α ⇒ [α] → WithPlus α
fromList = WithPlus ∘ S.fromList

singleton ∷ α → WithPlus α
singleton = WithPlus ∘ S.singleton

toString ∷ HumanReadable α ⇒ WithPlus α → String
toString xs
      | null xs = "None"
      | otherwise = (intercalate "+" ∘ map HR.toString ∘ toList) xs

parseString ∷ (Ord α, HumanReadable α, MonadFail m) ⇒ String → m (WithPlus α)
parseString s
      | null s = pure (∅)
      | map toLower s ≡ "none" = pure (∅)
      | otherwise = fromList <$> traverse HR.parseString (toList (split (≡'+') s))

instance HumanReadable α ⇒ ToJSON (WithPlus α) where
    toJSON = String ∘ T.pack ∘ toString

instance (Ord α, HumanReadable α) ⇒ FromJSON (WithPlus α) where
    parseJSON (String s) = parseString (T.unpack s)
    parseJSON (Array a) = fromList <$> traverse HR.hrParseJSON (toList a)
    parseJSON v = typeMismatch "String or Array" v

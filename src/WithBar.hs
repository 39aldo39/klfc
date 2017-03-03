{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WithBar
    ( WithBar(..)
    , toString
    , parseString
    ) where

import BasePrelude
import Prelude.Unicode
import Util (HumanReadable, split, (>$>))
import qualified Util as HR (HumanReadable(..))

import Control.Monad.Fail (MonadFail)
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, withText)
import Data.Aeson.Types (Value(String))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T

newtype WithBar α = WithBar { getNonEmpty ∷ NonEmpty α }
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

toString ∷ HumanReadable α ⇒ WithBar α → String
toString = intercalate "|" ∘ map HR.toString ∘ toList

parseString ∷ (HumanReadable α, MonadFail m) ⇒ String → m (WithBar α)
parseString = split (≡ '|') >>> traverse HR.parseString >$> WithBar

instance HumanReadable α ⇒ ToJSON (WithBar α) where
    toJSON = String ∘ T.pack ∘ toString

instance HumanReadable α ⇒ FromJSON (WithBar α) where
    parseJSON = withText "String" (parseString ∘ T.unpack)

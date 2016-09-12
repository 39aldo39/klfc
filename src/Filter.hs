{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Filter
    ( Filter(..)
    ) where

import BasePrelude
import Prelude.Unicode
import Data.List.Unicode ((∋), (∌))
import Data.Monoid.Unicode ((⊕))
import Util (HumanReadable(..), split)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import Data.Aeson (FromJSON, parseJSON, withText)
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail

newtype Filter α = Filter { runFilter ∷ α → Bool }

parseFilter ∷ (Eq α, HumanReadable α, MonadFail m) ⇒ String → m (Filter α)
parseFilter =
    dropWhile (∈ ", ") >>>
    split (∈ ", ") >>> \(kind :| types) →
    traverse parseString (filter (not ∘ null) types) >>=
    toFilter kind
  where
    toFilter kind =
      case map toLower kind of
          "only" → pure ∘ Filter ∘ (∋)
          "no"   → pure ∘ Filter ∘ (∌)
          _ → const $ Fail.fail ("unknown filter kind ‘" ⊕ kind ⊕ "’")

instance (Eq α, HumanReadable α) ⇒ FromJSON (Filter α) where
    parseJSON = withText "filter" (parseFilter ∘ T.unpack)

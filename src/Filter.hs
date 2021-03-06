{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Filter
    ( Filter
    , runFilter
    ) where

import BasePrelude
import Prelude.Unicode
import Data.List.Unicode ((∋), (∌))
import Data.Monoid.Unicode ((⊕))
import Util (HumanReadable(..), split)

import qualified Data.Text as T
import Data.Aeson (FromJSON, parseJSON, withText)
import qualified Control.Monad.Fail as Fail

newtype Filter α = Filter { runFilter' ∷ α → All } deriving (Semigroup, Monoid)

runFilter ∷ Filter α → α → Bool
runFilter = fmap getAll ∘ runFilter'

parseFilter ∷ (Eq α, HumanReadable α, MonadFail m) ⇒ String → m (Filter α)
parseFilter =
    dropWhile (∈ ", ") >>>
    split (∈ ", ") >>> \(kind :| types) →
    traverse parseString (filter (not ∘ null) types) >>=
    toFilter kind
  where
    toFilter kind =
      case map toLower kind of
          "only" → pure ∘ Filter ∘ fmap All ∘ (∋)
          "no"   → pure ∘ Filter ∘ fmap All ∘ (∌)
          _ → const $ Fail.fail ("unknown filter kind ‘" ⊕ kind ⊕ "’")

instance (Eq α, HumanReadable α) ⇒ FromJSON (Filter α) where
    parseJSON = withText "filter" (parseFilter ∘ T.unpack)

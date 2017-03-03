{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Layout.DeadKey
    ( DeadKey(..)
    , StringMap
    , ChainedDeadKey(..)
    , ActionMap
    , ActionResult(..)
    , deadKeyToChainedDeadKey
    ) where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((⊕))
import Util (combineWithOnM, nubWithOnM)

import Control.Monad.Writer (MonadWriter, tell)
import Data.Aeson

type StringMap = [(String, String)]
data DeadKey = DeadKey
    { __dkName ∷ String
    , __baseChar ∷ Maybe Char
    , __stringMap ∷ StringMap
    } deriving (Eq, Show, Read)

instance ToJSON DeadKey where
    toJSON (DeadKey name baseChar stringMap) =
        object $ concat [["name" .= name], char, ["stringMap" .= stringMap]]
      where
        char =
          case baseChar of
            Nothing → []
            Just c  → ["baseChar" .= c]
instance FromJSON DeadKey where
    parseJSON = withObject "dead key" $ \o →
        DeadKey
          <$> o .:  "name"
          <*> o .:? "baseChar"
          <*> o .:  "stringMap"

data ActionResult = OutString String | Next ChainedDeadKey deriving (Eq, Show, Read)
type ActionMap = [(Char, ActionResult)]
data ChainedDeadKey = ChainedDeadKey
    { __cdkName ∷ String
    , __cdkBaseChar ∷ Maybe Char
    , __actionMap ∷ ActionMap
    } deriving (Eq, Show, Read)

deadKeyToChainedDeadKey ∷ MonadWriter [String] m
                        ⇒ DeadKey → m ChainedDeadKey
deadKeyToChainedDeadKey (DeadKey name baseChar stringMap) =
    let name' = filter (≢ ':') name
    in  ChainedDeadKey name' baseChar <$> stringMapToActionMap (name' ⊕ ":") stringMap

combineChainedDeadKey ∷ MonadWriter [String] m
                      ⇒ ChainedDeadKey → ChainedDeadKey → m ChainedDeadKey
combineChainedDeadKey (ChainedDeadKey name baseChar1 actionMap1) (ChainedDeadKey _ baseChar2 actionMap2) =
    ChainedDeadKey name (baseChar1 <|> baseChar2) <$> combineActionMap name actionMap1 actionMap2

combineActionMap ∷ MonadWriter [String] m
                 ⇒ String → ActionMap → ActionMap → m ActionMap
combineActionMap name = combineWithOnM (foldlM (combineAction name)) fst

combineAction ∷ MonadWriter [String] m
              ⇒ String → (Char, ActionResult) → (Char, ActionResult) → m (Char, ActionResult)
combineAction _ (c, Next next1) (_, Next next2) = (,) c ∘ Next <$> combineChainedDeadKey next1 next2
combineAction name (c, Next next) (_, OutString s) = (c, Next next) <$ tell ["the output ‘" ⊕ s ⊕ "’ is ignored in a conflicting definition in dead key ‘" ⊕ name ⊕ [c] ⊕ "’"]
combineAction name (c, OutString s) (_, Next next) = (c, Next next) <$ tell ["the output ‘" ⊕ s ⊕ "’ is ignored in a conflicting definition in dead key ‘" ⊕ name ⊕ [c] ⊕ "’"]
combineAction name (c, OutString s1) (_, OutString s2) = (c, OutString s1) <$ tell
  (bool ["the output ‘" ⊕ s2 ⊕ "’ is ignored in favor of ‘" ⊕ s1 ⊕ "’ in a conflicting definition in dead key ‘" ⊕ name ⊕ [c] ⊕ "’"] [] (s1 ≡ s2))

stringMapToActionMap ∷ MonadWriter [String] m
                     ⇒ String → StringMap → m ActionMap
stringMapToActionMap name =
    concatMap (stringToActionMap name) >>>
    nubWithOnM (foldlM (combineAction name)) fst

stringToActionMap ∷ String → (String, String) → ActionMap
stringToActionMap _ ("", _) = []
stringToActionMap _ ([x], outString) = [(x, OutString outString)]
stringToActionMap name (x:xs, outString) = [(x, Next cdk)]
  where
    cdk = ChainedDeadKey name' Nothing (stringToActionMap name' (xs, outString))
    name' = name ⊕ [x]

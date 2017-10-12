{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Layout.DeadKey
    ( DeadKey(..)
    , ActionMap
    , ActionResult(..)
    , stringMapToActionMap
    , actionMapToStringMap
    , getModifiedChars
    ) where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((⊕))
import Util (combineWithOnM, nubWithOnM)

import Data.Aeson
import Data.Set (Set)
import qualified Data.Set as S
import Lens.Micro.Platform (over, _1)
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail (fail)

type StringMap = [(String, String)]
data ActionResult = OutString String | Next DeadKey deriving (Eq, Show, Read)
type ActionMap = [(Char, ActionResult)]
data DeadKey = DeadKey
    { __dkName ∷ String
    , __baseChar ∷ Maybe Char
    , __actionMap ∷ ActionMap
    } deriving (Eq, Show, Read)

instance ToJSON DeadKey where
    toJSON (DeadKey name baseChar actionMap) =
        object $ concat [["name" .= name], char, ["stringMap" .= actionMapToStringMap actionMap]]
      where
        char =
          case baseChar of
            Nothing → []
            Just c  → ["baseChar" .= c]
instance FromJSON DeadKey where
    parseJSON = withObject "dead key" $ \o → do
        name      ← o .:  "name"
        baseChar  ← o .:? "baseChar"
        stringMap ← o .:  "stringMap"
        actionMap ← stringMapToActionMap name stringMap
        pure $ DeadKey name baseChar actionMap

actionMapToStringMap ∷ ActionMap → StringMap
actionMapToStringMap = concatMap actionToString
  where
    actionToString (x, OutString s) = [([x], s)]
    actionToString (x, Next (DeadKey _ _ actionMap)) = over (traverse ∘ _1) (x:) (actionMapToStringMap actionMap)

combineDeadKey ∷ MonadFail m ⇒ DeadKey → DeadKey → m DeadKey
combineDeadKey (DeadKey name baseChar1 actionMap1) (DeadKey _ baseChar2 actionMap2) =
    DeadKey name (baseChar1 <|> baseChar2) <$> combineActionMap name actionMap1 actionMap2

combineActionMap ∷ MonadFail m ⇒ String → ActionMap → ActionMap → m ActionMap
combineActionMap name = combineWithOnM (foldlM (combineAction name)) fst

combineAction ∷ MonadFail m ⇒ String → (Char, ActionResult) → (Char, ActionResult) → m (Char, ActionResult)
combineAction _ (c, Next next1) (_, Next next2) = (,) c ∘ Next <$> combineDeadKey next1 next2
combineAction name (c, _) _ = Fail.fail ("conflicting definition in dead key ‘" ⊕ name ⊕ [c] ⊕ "’")

stringMapToActionMap ∷ MonadFail m ⇒ String → StringMap → m ActionMap
stringMapToActionMap name =
    concatMap (stringToActionMap name) >>>
    nubWithOnM (foldlM (combineAction name)) fst

stringToActionMap ∷ String → (String, String) → ActionMap
stringToActionMap _ ("", _) = []
stringToActionMap _ ([x], outString) = [(x, OutString outString)]
stringToActionMap name (x:xs, outString) = [(x, Next dk)]
  where
    dk = DeadKey name' Nothing (stringToActionMap name' (xs, outString))
    name' = name ⊕ [x]

getModifiedChars ∷ DeadKey → Set Char
getModifiedChars = S.unions ∘ map getModifiedChars' ∘ __actionMap
  where
    getModifiedChars' (x, OutString _) = S.singleton x
    getModifiedChars' (x, Next dk) = S.insert x (getModifiedChars dk)

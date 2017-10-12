{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Layout.DeadKey
    ( DeadKey'(..)
    , ActionResult'(..)
    , ActionMap'
    , stringMapToActionMap
    , actionMapToStringMap
    , getModifiedLetters
    ) where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((⊕))
import Util (HumanReadable(..), combineWithOnM, nubWithOnM)

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Lens.Micro.Platform (over, _1)
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail (fail)

type StringMap' l = [([l], String)]
data ActionResult' l = OutString String | Next (DeadKey' l) deriving (Eq, Ord, Show, Read)
type ActionMap' l = [(l, ActionResult' l)]
data DeadKey' l = DeadKey
    { __dkName ∷ String
    , __baseChar ∷ Maybe Char
    , __actionMap ∷ ActionMap' l
    } deriving (Eq, Ord, Show, Read)

instance ToJSON l ⇒ ToJSON (DeadKey' l) where
    toJSON (DeadKey name baseChar actionMap) =
        object $ concat [["name" .= name], char, ["stringMap" .= actionMapToStringMap actionMap]]
      where
        char =
          case baseChar of
            Nothing → []
            Just c  → ["baseChar" .= c]
instance (Eq l, HumanReadable l, FromJSON l, Show l) ⇒ FromJSON (DeadKey' l) where
    parseJSON = withObject "dead key" $ \o → do
        name      ← o .:  "name"
        baseChar  ← o .:? "baseChar"
        stringMap ← over (traverse ∘ _1) letterListToList <$> o .: "stringMap"
        actionMap ← stringMapToActionMap name stringMap
        pure $ DeadKey name baseChar actionMap

data LetterList l = LetterList [l]

letterListToList ∷ LetterList l → [l]
letterListToList (LetterList ls) = ls

instance FromJSON l ⇒ FromJSON (LetterList l) where
    parseJSON (Array a) = LetterList <$> traverse parseJSON (toList a)
    parseJSON (String s) = LetterList <$> traverse parseJSON (map String (T.chunksOf 1 s))
    parseJSON v = typeMismatch "String or Array" v

actionMapToStringMap ∷ ActionMap' l → StringMap' l
actionMapToStringMap = concatMap actionToString
  where
    actionToString (x, OutString s) = [([x], s)]
    actionToString (x, Next (DeadKey _ _ actionMap)) = over (traverse ∘ _1) (x:) (actionMapToStringMap actionMap)

combineDeadKey ∷ (MonadFail m, Eq l, HumanReadable l, Show l) ⇒ DeadKey' l → DeadKey' l → m (DeadKey' l)
combineDeadKey (DeadKey name baseChar1 actionMap1) (DeadKey _ baseChar2 actionMap2) =
    DeadKey name (baseChar1 <|> baseChar2) <$> combineActionMap name actionMap1 actionMap2

combineActionMap ∷ (MonadFail m, Eq l, HumanReadable l, Show l) ⇒ String → ActionMap' l → ActionMap' l → m (ActionMap' l)
combineActionMap name = combineWithOnM (foldlM (combineAction name)) fst

combineAction ∷ (MonadFail m, Eq l, HumanReadable l, Show l) ⇒ String → (l, ActionResult' l) → (l, ActionResult' l) → m (l, ActionResult' l)
combineAction _ (x, Next next1) (_, Next next2) = (,) x ∘ Next <$> combineDeadKey next1 next2
combineAction name (x, _) _ = Fail.fail ("conflicting definition in dead key ‘" ⊕ name ⊕ ":" ⊕ toString x ⊕ "’")

stringMapToActionMap ∷ (MonadFail m, Eq l, HumanReadable l, Show l) ⇒ String → StringMap' l → m (ActionMap' l)
stringMapToActionMap name =
    concatMap (stringToActionMap name) >>>
    nubWithOnM (foldlM (combineAction name)) fst

stringToActionMap ∷ HumanReadable l ⇒ String → ([l], String) → ActionMap' l
stringToActionMap _ ([], _) = []
stringToActionMap _ ([x], outString) = [(x, OutString outString)]
stringToActionMap name (x:xs, outString) = [(x, Next dk)]
  where
    dk = DeadKey name' Nothing (stringToActionMap name' (xs, outString))
    name' = name ⊕ ":" ⊕ toString x

getModifiedLetters ∷ Ord l ⇒ DeadKey' l → Set l
getModifiedLetters = S.unions ∘ map getModifiedLetters' ∘ __actionMap
  where
    getModifiedLetters' (x, OutString _) = S.singleton x
    getModifiedLetters' (x, Next dk) = S.insert x (getModifiedLetters dk)

{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Layout.DeadKey
    ( DeadKey'(..)
    , BaseChar'(..)
    , ActionResult'(..)
    , ActionMap'
    , stringMapToActionMap
    , actionMapToStringMap
    , combineDeadKey
    , getModifiedLetters
    ) where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((⊕))
import Util (HumanReadable(..), combineWithOnM, nubWithOnM)

import Control.Monad.Writer (MonadWriter, tell, runWriter)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Lens.Micro.Platform (over, _1)
import qualified Control.Monad.Fail as Fail (fail)

data BaseChar' p = BaseNo | BaseChar Char | BasePreset p deriving (Eq, Ord, Show, Read)
type StringMap' l = [([l], String)]
data ActionResult' l p = OutString String | Next (DeadKey' l p) deriving (Eq, Ord, Show, Read)
type ActionMap' l p = [(l, ActionResult' l p)]
data DeadKey' l p = DeadKey
    { __dkName ∷ String
    , __baseChar ∷ BaseChar' p
    , __actionMap ∷ ActionMap' l p
    } deriving (Eq, Ord, Show, Read)

instance (ToJSON l, HumanReadable p) ⇒ ToJSON (DeadKey' l p) where
    toJSON (DeadKey name baseChar actionMap) =
        object $ concat
            [ ["name" .= name]
            , char
            , ["stringMap" .= over (traverse ∘ _1) LetterList (actionMapToStringMap actionMap)]
            ]
      where
        char =
          case baseChar of
            BaseNo → []
            BaseChar c → ["baseChar" .= c]
            BasePreset p → ["baseChar" .= toString p]
instance (Eq l, HumanReadable l, FromJSON l, Show l, HumanReadable p) ⇒ FromJSON (DeadKey' l p) where
    parseJSON = withObject "dead key" $ \o → do
        baseChar  ← o .:? "baseChar" >>= readBaseChar
        name      ← getName o baseChar
        stringMap ← over (traverse ∘ _1) letterListToList <$> o .: "stringMap"
        let (actionMap, warnings) = runWriter (stringMapToActionMap name stringMap)
        traverse_ Fail.fail warnings
        pure $ DeadKey name baseChar actionMap
      where
        readBaseChar Nothing = pure BaseNo
        readBaseChar (Just [x]) = pure (BaseChar x)
        readBaseChar (Just xs) =
          case parseString xs of
            Nothing → Fail.fail ("‘" ⊕ xs ⊕ "’" ⊕ "is not a single character, nor a known predefined dead key")
            Just d  → pure (BasePreset d)
        getName o (BasePreset p) = o .:? "name" .!= toString p
        getName o _ = o .: "name"

data LetterList l = LetterList [l]

letterListToList ∷ LetterList l → [l]
letterListToList (LetterList ls) = ls

instance ToJSON l ⇒ ToJSON (LetterList l) where
    toJSON (LetterList ls) =
        maybe (toJSON ls) (String ∘ mconcat) (traverse (toText ∘ toJSON) ls)
      where
        toText (String s) | T.length s ≡ 1 = Just s
        toText _ = Nothing

instance FromJSON l ⇒ FromJSON (LetterList l) where
    parseJSON (Array a) = LetterList <$> traverse parseJSON (toList a)
    parseJSON (String s) = LetterList <$> traverse parseJSON (map String (T.chunksOf 1 s))
    parseJSON v = typeMismatch "String or Array" v

actionMapToStringMap ∷ ActionMap' l p → StringMap' l
actionMapToStringMap = concatMap actionToString
  where
    actionToString (x, OutString s) = [([x], s)]
    actionToString (x, Next (DeadKey _ _ actionMap)) = over (traverse ∘ _1) (x:) (actionMapToStringMap actionMap)

combineDeadKey ∷ (MonadWriter [String] m, Eq l, HumanReadable l, Show l)
               ⇒ DeadKey' l p → DeadKey' l p → m (DeadKey' l p)
combineDeadKey (DeadKey name baseChar1 actionMap1) (DeadKey _ baseChar2 actionMap2) =
    DeadKey name baseChar <$> combineActionMap name actionMap1 actionMap2
  where
    baseChar =
      case baseChar1 of
        BaseNo → baseChar2
        x → x

combineActionMap ∷ (MonadWriter [String] m, Eq l, HumanReadable l, Show l)
                 ⇒ String → ActionMap' l p → ActionMap' l p → m (ActionMap' l p)
combineActionMap name = combineWithOnM (foldlM (combineAction name)) fst

combineAction ∷ (MonadWriter [String] m, Eq l, HumanReadable l, Show l)
              ⇒ String → (l, ActionResult' l p) → (l, ActionResult' l p) → m (l, ActionResult' l p)
combineAction _ (x, Next next1) (_, Next next2) = (,) x ∘ Next <$> combineDeadKey next1 next2
combineAction name result@(x, _) _ = result <$ tell ["conflicting definition in dead key ‘" ⊕ name ⊕ ":" ⊕ toString x ⊕ "’"]

stringMapToActionMap ∷ (MonadWriter [String] m, Eq l, HumanReadable l, Show l)
                     ⇒ String → StringMap' l → m (ActionMap' l p)
stringMapToActionMap name =
    concatMap (stringToActionMap name) >>>
    nubWithOnM (foldlM (combineAction name)) fst

stringToActionMap ∷ HumanReadable l ⇒ String → ([l], String) → ActionMap' l p
stringToActionMap _ ([], _) = []
stringToActionMap _ ([x], outString) = [(x, OutString outString)]
stringToActionMap name (x:xs, outString) = [(x, Next dk)]
  where
    dk = DeadKey name' BaseNo (stringToActionMap name' (xs, outString))
    name' = name ⊕ ":" ⊕ toString x

getModifiedLetters ∷ Ord l ⇒ DeadKey' l p → Set l
getModifiedLetters = S.unions ∘ map getModifiedLetters' ∘ __actionMap
  where
    getModifiedLetters' (x, OutString _) = S.singleton x
    getModifiedLetters' (x, Next dk) = S.insert x (getModifiedLetters dk)

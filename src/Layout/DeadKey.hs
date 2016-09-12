{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Layout.DeadKey
    ( DeadKey(DeadKey)
    , StringMap
    , __dkName
    , __baseChar
    , __stringMap
    ) where

import BasePrelude

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

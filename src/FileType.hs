{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module FileType
    ( FileType(..)
    ) where

import BasePrelude
import Util (HumanReadable(..))

data FileType
    = Json
    | Xkb
    | Pkl
    | Klc
    | Keylayout
    deriving (Eq, Show, Read, Enum, Bounded)

typeAndString ∷ [(FileType, String)]
typeAndString =
    [ (Json, "JSON")
    , (Xkb, "XKB")
    , (Pkl, "PKL")
    , (Klc, "KLC")
    , (Keylayout, "keylayout")
    ]

instance HumanReadable FileType where
    typeName _ = "file type"
    stringList = typeAndString

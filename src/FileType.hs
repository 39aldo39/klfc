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
    | Tmk
    deriving (Eq, Show, Read, Enum, Bounded)

typeAndString ∷ [(FileType, String)]
typeAndString =
    [ (Json, "JSON")
    , (Xkb, "XKB")
    , (Pkl, "PKL")
    , (Klc, "KLC")
    , (Keylayout, "keylayout")
    , (Tmk, "TMK")
    ]

instance HumanReadable FileType where
    typeName _ = "file type"
    stringList = typeAndString

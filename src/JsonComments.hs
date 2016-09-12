{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonComments
    ( removeJsonComments
    ) where

import BasePrelude
import Prelude.Unicode

import qualified Data.ByteString.Lazy.Char8 as BL

-- Preserve line numbers
removeJsonComments ∷ BL.ByteString → BL.ByteString
removeJsonComments = BL.unlines ∘ map removeJsonComment ∘ BL.lines

removeJsonComment ∷ BL.ByteString → BL.ByteString
removeJsonComment s
  | isCommentLine s = ""
  | otherwise       = s

isCommentLine ∷ BL.ByteString → Bool
isCommentLine = BL.isPrefixOf "//" ∘ BL.dropWhile isSpace

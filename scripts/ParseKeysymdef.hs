#!/usr/bin/runhaskell
{-# LANGUAGE UnicodeSyntax #-}
module ParseKeysymdef where

import Prelude.Unicode

import Data.Char (chr)
import Text.Read (readMaybe)
import qualified Data.Text.Lazy.IO as L (readFile)
import Control.Applicative (liftA2)
import Text.Megaparsec
import Text.Megaparsec.Text.Lazy (Parser)

symLine ∷ Parser [(String, Char)]
symLine = (:[]) <$> liftA2 (,)
          (string "#define XK_" *> many (alphaNumChar <|> char '_') <* space <* many alphaNumChar <* space)
          (fmap f (string "/*" *> space *> string "U+" *> many alphaNumChar <* manyTill anyChar (try (string "*/")) <* eol))
    where f xs = maybe '\0' chr (readMaybe ('0':'x':xs))

symLines ∷ Parser [(String, Char)]
symLines = concat <$> many (try symLine <|> [] <$ manyTill anyChar (char '\n'))

printLines ∷ [(String, Char)] → String
printLines = unlines ∘ map (\(s, c) → "    , ('" ⧺ [c] ⧺ "', " ⧺ show s ⧺ ")")

parseFromFile ∷ Parser α → String → IO (Either (ParseError Char Dec) α)
parseFromFile p fname = parse p "" <$> L.readFile fname

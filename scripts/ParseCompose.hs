#!/usr/bin/runhaskell
{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
module Main where

import Prelude.Unicode
import BasePrelude hiding (try)

import Lookup.Linux (charAndString)
import Util (escape)

import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import qualified Data.Text.Lazy.IO as L (readFile)
import Text.Megaparsec
import Text.Megaparsec.Text.Lazy (Parser)

main ∷ IO ()
main =
    parseFromFile composeLines "/usr/share/X11/locale/en_US.UTF-8/Compose" >>=
    either (putStr ∘ parseErrorPretty) printLines

keysym ∷ Parser (Maybe Char)
keysym = parseKeysym <$>
    (char '<' *> many (alphaNumChar <|> char '_') <* char '>' <* space)

parseKeysym ∷ String → Maybe Char
parseKeysym "U" = Just 'U'
parseKeysym ('U':xs) | all isHexDigit xs = Just (chr (read ("0x" ⧺ xs)))
parseKeysym s = lookup s (map swap charAndString)

composeLine ∷ Parser [(String, String)]
composeLine = (\(inS', outS) → maybe [] (\inS → [(inS, outS)]) inS') <$> liftA2 (,)
          (sequence <$> (string "<Multi_key>" *> space *> many keysym))
          ((\s → read ('"' : s ⧺ ['"'])) <$> (char ':' *> space *> char '"' *> manyTill anyChar (char '"') <* manyTill anyChar eol))

composeLines ∷ Parser [(String, String)]
composeLines = concat <$> many (composeLine <|> [] <$ manyTill anyChar eol)

printLines ∷ [(String, String)] → IO ()
--printLines = unlines ∘ map (\(inS, outS) → "[ " ⧺ escape inS ⧺ ", " ⧺ escape outS ⧺ " ],")
printLines = traverse_ (\x → BL.putStr (encode x) >> putStrLn ",")

parseFromFile ∷ Parser α → String → IO (Either (ParseError Char Dec) α)
parseFromFile p fname = parse p "" <$> L.readFile fname

#!/usr/bin/env runhaskell
{-# LANGUAGE UnicodeSyntax #-}
module ParseKeysymdef where

import Prelude.Unicode

import Data.Char (chr)
import Data.Void (Void)
import Text.Read (readMaybe)
import Control.Applicative (liftA2)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

main ∷ IO ()
main =
    parseFromFile symLines "/usr/include/X11/keysymdef.h" >>=
    either (print ∘ errorBundlePretty) (mapM_ (\(s,c) → putStrLn (replicate 4 ' ' ⧺ ", ('" ⧺ charToString c ⧺ "', " ⧺ show s ⧺ ")")))

symLine ∷ Parser [(String, Char)]
symLine = (:[]) <$> liftA2 (,)
          (string "#define XK_" *> many (alphaNumChar <|> char '_') <* space <* many alphaNumChar <* space)
          (fmap f (string "/*" *> space *> string "U+" *> many alphaNumChar <* manyTill anySingle eol))
    where f xs = maybe '\0' chr (readMaybe ('0':'x':xs))

symLines ∷ Parser [(String, Char)]
symLines = concat <$> many (try symLine <|> [] <$ manyTill anySingle eol)

printLines ∷ [(String, Char)] → String
printLines = unlines ∘ map (\(s, c) → "    , ('" ⧺ charToString c ⧺ "', " ⧺ show s ⧺ ")")
  where

parseFromFile ∷ Parser α → String → IO (Either (ParseErrorBundle String Void) α)
parseFromFile p fname = parse p "" <$> readFile fname

charToString ∷ Char → String
charToString '\'' = "\\\'"
charToString '\\' = "\\\\"
charToString '\173' = "\\173"
charToString c = [c]

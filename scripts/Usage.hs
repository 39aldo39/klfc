#!/usr/bin/runhaskell
{-# LANGUAGE UnicodeSyntax #-}
import Prelude.Unicode
import Data.Bool (bool)
import Data.Monoid.Unicode ((⊕))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Control.Applicative (liftA2)

modifyLines ∷ Bool → [String] → [String]
modifyLines False [] = []
modifyLines True  [] = ["```"]
modifyLines _ ("Available options:":ys) = ["### Available options ###", "```"] ⧺ modifyLines True ys
modifyLines _ (('U':'s':'a':'g':'e':':':' ':xs):ys) = ["```", xs, "```"] ⧺ modifyLines False ys
modifyLines inCode (xs:ys)
  | lastElm ≡ Just ':' = endCodeLines ⧺ ["#### " ⊕ init xs' ⊕ " ####", "```"] ⧺ modifyLines True ys
  | all isSpace' xs = endCodeLines ⧺ [""] ⧺ modifyLines False ys
  | otherwise = [drop 2 xs] ⧺ modifyLines inCode ys
  where
    xs' = dropWhileEnd isSpace' (dropWhile isSpace' xs)
    isSpace' = liftA2 (∨) isSpace (≡'\b')
    lastElm = if null xs' then Nothing else Just (last xs')
    endCodeLines = bool [] ["```"] inCode

addSubTitles ∷ [String] → [String]
addSubTitles [] = []
addSubTitles (x:x'@('#':_):xs)
  | all isSpace x = x : x' : addSubTitles xs
  | otherwise = x : ("#" ⊕ x' ⊕ "#") : addSubTitles xs
addSubTitles (x:xs) = x : addSubTitles xs

main ∷ IO ()
main = getContents >>= mapM_ putStrLn ∘ addSubTitles ∘ (modifyLines False) ∘ drop 1 ∘ lines

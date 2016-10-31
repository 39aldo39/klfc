#!/usr/bin/runhaskell
{-# LANGUAGE UnicodeSyntax #-}
import Prelude.Unicode
import Data.Monoid.Unicode ((⊕))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Control.Applicative (liftA2)

modifyLine ∷ String → [String]
modifyLine "Available options:" = ["### Available options ###", ""]
modifyLine ('U':'s':'a':'g':'e':':':' ':xs) = [replicate 4 ' ' ⊕ xs]
modifyLine xs
  | lastElm ≡ Just ':' = ["#### " ⊕ init xs' ⊕ " ####"]
  | all isSpace' xs = [""]
  | otherwise = [replicate 2 ' ' ⊕ xs]
  where
    xs' = dropWhileEnd isSpace' (dropWhile isSpace' xs)
    isSpace' = liftA2 (∨) isSpace (≡'\b')
    lastElm = if null xs' then Nothing else Just (last xs')

addSubTitles ∷ [String] → [String]
addSubTitles [] = []
addSubTitles (x:x'@('#':_):xs)
  | all isSpace x = x : x' : addSubTitles xs
  | otherwise = x : ("#" ⊕ x' ⊕ "#") : addSubTitles xs
addSubTitles (x:xs) = x : addSubTitles xs

main ∷ IO ()
main = getContents >>= mapM_ putStrLn ∘ addSubTitles ∘ concatMap modifyLine ∘ drop 1 ∘ lines

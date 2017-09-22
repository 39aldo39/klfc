#!/usr/bin/runhaskell
{-# LANGUAGE UnicodeSyntax #-}

module ToMarkdownList where

import Prelude.Unicode
import Data.Monoid.Unicode ((⊕))

import Data.Maybe (fromMaybe)
import Data.Char (isLetter)
import Data.Void (Void)
import Data.List (intercalate, groupBy)
import Data.Function (on)
import Text.Read (readMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

type MdList = [Either Category PartOfMdList]
type Category = String
data PartOfMdList = PartOfMdList PartTitle [ListElement] deriving Show
type PartTitle = String
type ListElement = [String]

type Title = String
mdListToMd ∷ Title → MdList → [String]
mdListToMd title = (:) title ∘ concatMap (either catToMd partToMd)

catToMd ∷ Category → [String]
catToMd cat = ["", "##### " ⊕ cat ⊕ " #####"]

partToMd ∷ PartOfMdList → [String]
partToMd (PartOfMdList title elems) = "" : "###### " ⊕ title ⊕ " ######" : map (("* " ⊕) . elemToMd) elems

elemToMd ∷ ListElement → String
elemToMd = intercalate ", " ∘ map escape

escape ∷ String → String
escape xs
  | '`' ∈ xs = '`' : xs' ⧺ "`"
  | otherwise = xs'
  where xs' = '`' : escape' xs ⧺ "`"

escape' ∷ String → String
escape' "" = ""
escape' "`" = "` "
escape' (x:xs) = x : escape' xs

type HaskellList = [Either Category PartOfHaskellList]
data PartOfHaskellList = PartOfHaskellList PartTitle [HaskellListElement] deriving Show
type HaskellListElement = (String, String)

haskellListToMdList ∷ HaskellList → MdList
haskellListToMdList = map (fmap partOfHaskellListToPartOfMdList)

partOfHaskellListToPartOfMdList ∷ PartOfHaskellList → PartOfMdList
partOfHaskellListToPartOfMdList (PartOfHaskellList title tuples) = PartOfMdList title (haskellListElementsToMdListElements tuples)

haskellListElementsToMdListElements ∷ [HaskellListElement] → [ListElement]
haskellListElementsToMdListElements = map (map (\(_,s) → fromMaybe (e s) (readMaybe s))) ∘ groupBy ((≡) `on` fst)
  where e s = error ("cannot read string " ⊕ s)

data DefType = Data | List
type DefName = String
haskellList ∷ DefType → DefName → Parser HaskellList
haskellList def name =
    uninterestingStuff def name *>
    some (try (Left <$> category) <|> try (Right <$> partOfHaskellList def))

uninterestingStuff ∷ DefType → DefName → Parser ()
uninterestingStuff def name = do
    nameGot ← words <$> readLine
    if (take 2 nameGot ≡ (case def of Data → ["data", name]; List → [name, "="]))
      then pure ()
      else uninterestingStuff def name

category ∷ Parser Category
category = commentLine <* eol

partOfHaskellList ∷ DefType → Parser PartOfHaskellList
partOfHaskellList def = PartOfHaskellList <$> commentLine <*> some (try (haskellListElement def))

haskellListElement ∷ DefType → Parser HaskellListElement
haskellListElement Data = do
    action ← space *> oneOf "=|" *> space *> someTill anyChar spaceChar
    pure (action, show action)
haskellListElement List = do
    manyTill (noneOf "\r\n") (char '(')
    pos ← someTill anyChar (char ',') <* space
    string ← safeInit <$> readLine
    pure (pos, string)
  where
    safeInit [] = []
    safeInit xs = init xs

commentLine ∷ Parser String
commentLine = space *> string "--" *> space *> readLine

readLine ∷ Parser String
readLine = manyTill anyChar eol

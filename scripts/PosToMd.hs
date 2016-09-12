#!/usr/bin/runhaskell -iscripts
{-# LANGUAGE UnicodeSyntax #-}

module PosToMd where

import Prelude.Unicode
import ToMarkdownList
import System.IO (hPutStrLn, stderr)
import qualified Data.Text.Lazy.IO as L (getContents)
import Text.Megaparsec (parse)

main ∷ IO ()
main = do
    text ← L.getContents
    let list = haskellListToMdList <$> parse (haskellList List "posAndString") "" text
    let info = mdListToMd "The supported positions are:" <$> list
    either (hPutStrLn stderr ∘ show) (mapM_ putStrLn) info

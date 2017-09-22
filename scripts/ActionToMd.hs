#!/usr/bin/runhaskell -iscripts
{-# LANGUAGE UnicodeSyntax #-}

module ActionToMd where

import Prelude.Unicode
import ToMarkdownList
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (parse)

main ∷ IO ()
main = do
    text ← getContents
    let list = haskellListToMdList <$> parse (haskellList Data "Action") "" text
    let info = mdListToMd "The supported actions are:" <$> list
    either (hPutStrLn stderr ∘ show) (mapM_ putStrLn) info

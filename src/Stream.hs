{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Stream
    ( Stream(..)
    , toFname
    , readStream
    , writeStream
    ) where

import BasePrelude hiding (readFile, writeFile)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IOData (IOData, readFile, writeFile, hGetContents, hPut)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (stdin)

data Stream
    = Standard
    | File FilePath
    deriving (Show, Read)

toFname ∷ Stream → String
toFname Standard = "stdin"
toFname (File fname) = fname

readStream ∷ (MonadIO m, IOData α) ⇒ Stream → m α
readStream Standard = hGetContents stdin
readStream (File fname) = readFile fname

writeStream ∷ (MonadIO m, IOData α) ⇒ Stream → α → m ()
writeStream Standard = hPut stdin
writeStream (File fname) = \text → do
    liftIO $ createDirectoryIfMissing True (takeDirectory fname)
    writeFile fname text

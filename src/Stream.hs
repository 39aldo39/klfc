{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Stream
    ( Stream(..)
    , toFname
    , readStream
    , writeStream
    , writeStream'
    , writeFileStream
    , writeFileStream'
    ) where

import BasePrelude hiding (readFile, writeFile, hGetContents)
import Prelude.Unicode

import Data.IOData (IOData, readFile, writeFile, hGetContents, hPut)
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, listDirectory, removeFile, removeDirectory)
import System.FilePath (takeDirectory)

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
writeStream Standard = hPut stdout
writeStream (File fname) = \text → do
    liftIO $ createDirectoryIfMissing True (takeDirectory fname)
    writeFile fname text

writeStream' ∷ (MonadIO m, IOData α) ⇒ Stream → Maybe α → m ()
writeStream' Standard Nothing = pure ()
writeStream' Standard (Just text) = hPut stdout text
writeStream' (File fname) Nothing = do
    liftIO $ do
        exists ← doesFileExist fname
        when exists (removeFile fname)
    liftIO $ do
        let dir = takeDirectory fname
        exists ← doesDirectoryExist dir
        when exists $ do
            isEmpty ← null <$> listDirectory dir
            when isEmpty (removeDirectory dir)
writeStream' (File fname) (Just text) = do
    liftIO $ createDirectoryIfMissing True (takeDirectory fname)
    writeFile fname text

writeFileStream ∷ (MonadIO m, IOData α) ⇒ FilePath → α → m ()
writeFileStream = writeStream ∘ File

writeFileStream' ∷ (MonadIO m, IOData α) ⇒ FilePath → Maybe α → m ()
writeFileStream' = writeStream' ∘ File

{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Stream
    ( Stream(Standard, File)
    , ReadStream
    , WriteStream
    , readStream
    , writeStream
    ) where

import BasePrelude

import qualified Data.ByteString as B (ByteString, getContents, readFile)
import qualified Data.ByteString.Lazy as BL (ByteString, getContents, readFile)
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as L (Text)
import qualified Data.Text.Lazy.IO as L (getContents, putStrLn, readFile, writeFile)
import qualified Data.Text.IO as T (getContents, putStrLn, readFile, writeFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

data Stream
    = Standard
    | File FilePath
    deriving (Show, Read)

class ReadStream α where
    readStream ∷ Stream → IO α

class WriteStream α where
    writeStream ∷ Stream → α → IO ()

defReadStream ∷ IO α → (FilePath → IO α) → Stream → IO α
defReadStream f _ Standard = f
defReadStream _ g (File fname) = g fname

defWriteStream ∷ (α → IO ()) → (FilePath → α → IO ()) → Stream → α → IO ()
defWriteStream f _ Standard = f
defWriteStream _ g (File fname) = \text → do
    createDirectoryIfMissing True (takeDirectory fname)
    g fname text

instance ReadStream String where
    readStream = defReadStream getContents readFile
instance WriteStream String where
    writeStream = defWriteStream putStrLn writeFile

instance ReadStream T.Text where
    readStream = defReadStream T.getContents T.readFile
instance WriteStream T.Text where
    writeStream = defWriteStream T.putStrLn T.writeFile

instance ReadStream L.Text where
    readStream = defReadStream L.getContents L.readFile
instance WriteStream L.Text where
    writeStream = defWriteStream L.putStrLn L.writeFile

instance ReadStream B.ByteString where
    readStream = defReadStream B.getContents B.readFile

instance ReadStream BL.ByteString where
    readStream = defReadStream BL.getContents BL.readFile

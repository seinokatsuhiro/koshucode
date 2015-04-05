{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.IOPoint
  ( -- * I/O Point
    IOPoint (..),
    ioPointType, ioPointText,
    ioPointFrom, ioPointList,

    -- * Code information
    CodePiece (..),
    codeEmpty, codeTextOf,
  ) where

import qualified Data.Generics                as G
import qualified Koshucode.Baala.Base.Prelude as B


-- ----------------------  IOPoint

data IOPoint
    = IOPointFile  FilePath
    | IOPointUri   String
    | IOPointText  String
    | IOPointStdin
    | IOPointStdout
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Name of I/O point, i.e., @\"file\"@, @\"url\"@, @\"text\"@,
--   @\"stdin\"@, or @\"stdout\"@.
ioPointType :: IOPoint -> String
ioPointType (IOPointFile _)     = "file"
ioPointType (IOPointUri  _)     = "url"
ioPointType (IOPointText _)     = "text"
ioPointType (IOPointStdin)      = "stdin"
ioPointType (IOPointStdout)     = "stdout"

ioPointText :: IOPoint -> String
ioPointText (IOPointFile file)  = file
ioPointText (IOPointUri  url)   = url
ioPointText (IOPointText text)  = text
ioPointText (IOPointStdin)      = "<stdin>"
ioPointText (IOPointStdout)     = "<stdout>"

ioPointFrom :: String -> IOPoint
ioPointFrom path
    | B.isPrefixOf "http://"  path  = IOPointUri  path
    | B.isPrefixOf "https://" path  = IOPointUri  path
    | B.isPrefixOf "ftp://"   path  = IOPointUri  path
    | otherwise                     = IOPointFile path

-- | Create I/O points from using stdin, texts itself, filenames, and urls.
ioPointList :: Bool -> [String] -> [FilePath] -> [IOPoint]
ioPointList stdin texts paths =
    B.consIf stdin IOPointStdin $
         IOPointText `map` texts ++
         ioPointFrom `map` paths


-- ----------------------  CodePiece

data CodePiece
    = CodePiece { codeNumber :: Int
                , codeName   :: IOPoint }
      deriving (Show, G.Data, G.Typeable)

instance Eq CodePiece where
    x == y = codeName x == codeName y

instance Ord CodePiece where
    x `compare` y = codeName x `compare` codeName y

-- | Empty code.
codeEmpty :: CodePiece
codeEmpty = codeTextOf ""

-- | Create text code.
codeTextOf :: String -> CodePiece
codeTextOf = CodePiece 0 . IOPointText


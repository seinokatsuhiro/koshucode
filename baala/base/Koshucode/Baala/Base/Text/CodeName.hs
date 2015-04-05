{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.CodeName
  ( -- * Name of code
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
    = CodeFile  FilePath
    | CodeUri   String
    | CodeText  String
    | CodeStdin
    | CodeStdout
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Name of code type, i.e., @\"file\"@, @\"url\"@, @\"text\"@,
--   @\"stdin\"@, or @\"stdout\"@.
ioPointType :: IOPoint -> String
ioPointType (CodeFile _)     = "file"
ioPointType (CodeUri  _)     = "url"
ioPointType (CodeText _)     = "text"
ioPointType (CodeStdin)      = "stdin"
ioPointType (CodeStdout)     = "stdout"

ioPointText :: IOPoint -> String
ioPointText (CodeFile file)  = file
ioPointText (CodeUri  url)   = url
ioPointText (CodeText text)  = text
ioPointText (CodeStdin)      = "<stdin>"
ioPointText (CodeStdout)     = "<stdout>"

ioPointFrom :: String -> IOPoint
ioPointFrom path
    | B.isPrefixOf "http://"  path  = CodeUri  path
    | B.isPrefixOf "https://" path  = CodeUri  path
    | B.isPrefixOf "ftp://"   path  = CodeUri  path
    | otherwise                     = CodeFile path

-- | Create sources from using stdin, texts itself, filenames, and urls.
ioPointList :: Bool -> [String] -> [String] -> [IOPoint]
ioPointList stdin texts paths =
    B.consIf stdin CodeStdin $
         CodeText    `map` texts ++
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
codeTextOf = CodePiece 0 . CodeText


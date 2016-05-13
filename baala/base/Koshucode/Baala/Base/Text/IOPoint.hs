{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | I/O point: file, standard input, direct text, etc.

module Koshucode.Baala.Base.Text.IOPoint
  ( -- * Named handle
    NamedHandle (..),

    -- * I/O point
    IOPoint (..),
    ioPointType, ioPointText,
    ioPointFrom, ioPointList,

    -- * Code information
    CodePiece (..),
    codeEmpty, codeTextOf,
  ) where

import qualified Data.Generics                 as G
import qualified System.IO                     as IO
import qualified Koshucode.Baala.Base.Prelude  as B


-- ----------------------  Named handle

-- | Named I/O handle.
data NamedHandle = NamedHandle
    { handleName :: String       -- ^ Name of handle
    , handle     :: IO.Handle    -- ^ I/O handle
    } deriving (G.Data, G.Typeable)

instance Show NamedHandle where
    show = handleName

instance Eq NamedHandle where
    h1 == h2 = handleName h1 == handleName h2

instance Ord NamedHandle where
    h1 `compare` h2 = handleName h1 `compare` handleName h2


-- ----------------------  IOPoint

data IOPoint
    = IOPointFile   FilePath FilePath       -- ^ Context directory and target path
    | IOPointUri    String                  -- ^ Universal resource identifier
    | IOPointText   (Maybe String) B.Bz     -- ^ Code itself
    | IOPointCustom String B.Bz             -- ^ Custom I/O
    | IOPointStdin                          -- ^ Sandard input
    | IOPointStdout                         -- ^ Sandard output
    | IOPointOutput NamedHandle             -- ^ Output handler
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Name of I/O point, i.e., @\"file\"@, @\"url\"@, @\"text\"@,
--   @\"stdin\"@, or @\"stdout\"@.
ioPointType :: IOPoint -> String
ioPointType (IOPointFile _ _)   = "file"
ioPointType (IOPointUri  _)     = "url"
ioPointType (IOPointText _ _)   = "text"
ioPointType (IOPointCustom _ _) = "custom"
ioPointType (IOPointStdin)      = "stdin"
ioPointType (IOPointStdout)     = "stdout"
ioPointType (IOPointOutput _)   = "output"

-- | Name of I/O point.
ioPointText :: IOPoint -> String
ioPointText (IOPointFile dir file)       = dir ++ file
ioPointText (IOPointUri  url)            = url
ioPointText (IOPointText (Just name) _)  = name
ioPointText (IOPointText (Nothing) _)    = "<text>"
ioPointText (IOPointCustom name _)       = name
ioPointText (IOPointStdin)               = "<stdin>"
ioPointText (IOPointStdout)              = "<stdout>"
ioPointText (IOPointOutput h)            = handleName h

-- | Create I/O point.
ioPointFrom :: FilePath -> FilePath -> IOPoint
ioPointFrom context path
    | B.isPrefixOf "http://"  path  = IOPointUri  path
    | B.isPrefixOf "https://" path  = IOPointUri  path
    | B.isPrefixOf "ftp://"   path  = IOPointUri  path
    | otherwise                     = IOPointFile context path

-- | Create I/O points from using stdin, texts itself, filenames, and urls.
ioPointList :: Bool -> [B.Bz] -> FilePath -> [FilePath] -> [IOPoint]
ioPointList stdin texts context paths =
    B.consIf stdin IOPointStdin $
         IOPointText Nothing `map` texts ++
         ioPointFrom context `map` paths


-- ----------------------  CodePiece

-- | A piece of code.
data CodePiece
    = CodePiece { codeNumber :: Int
                , codeName   :: IOPoint }
      deriving (Show, G.Data, G.Typeable)

instance Eq CodePiece where
    x == y
      | xn == 0 && yn == 0  = codeName x == codeName y
      | otherwise           = xn == yn
      where xn = codeNumber x
            yn = codeNumber y
                          

instance Ord CodePiece where
    x `compare` y
      | xn == 0 && yn == 0  = codeName x `compare` codeName y
      | otherwise           = xn `compare` yn
      where xn = codeNumber x
            yn = codeNumber y

-- | Empty code.
codeEmpty :: CodePiece
codeEmpty = codeTextOf ""

-- | Create text code.
codeTextOf :: B.Bz -> CodePiece
codeTextOf = CodePiece 0 . IOPointText Nothing


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | I/O point: file, standard input, direct text, etc.

module Koshucode.Baala.Base.IO.IOPoint
  ( -- * Named handle
    NamedHandle (..),

    -- * I/O point
    IOPoint (..),
    ioPointType, ioPointText,
    ioPointFrom, ioPointList,

    -- * Indexed I/O point
    IxIOPoint (..),
    nioFrom,
    Code, ToCode (..),
  ) where

import qualified System.IO                     as IO
import qualified Data.ByteString.Lazy          as Bz
import qualified Koshucode.Baala.Overture      as O
import qualified Koshucode.Baala.Base.List     as B
import qualified Koshucode.Baala.Base.Prelude  as B


-- ----------------------  Named handle

-- | Named I/O handle.
data NamedHandle = NamedHandle
    { handleName :: String       -- ^ Name of handle
    , handle     :: IO.Handle    -- ^ I/O handle
    }

instance Show NamedHandle where
    show = handleName

instance Eq NamedHandle where
    h1 == h2 = handleName h1 == handleName h2

instance Ord NamedHandle where
    h1 `compare` h2 = handleName h1 `compare` handleName h2


-- ----------------------  IOPoint

-- | I/O point: file, standard input, direct text, etc.
data IOPoint
    = IOPointFile   FilePath FilePath       -- ^ Context directory and target path
    | IOPointUri    String                  -- ^ Universal resource identifier
    | IOPointText   (Maybe String) Code     -- ^ Code itself
    | IOPointCustom String B.Bz             -- ^ Custom I/O
    | IOPointStdin                          -- ^ Sandard input
    | IOPointStdout                         -- ^ Sandard output
    | IOPointOutput NamedHandle             -- ^ Output handler
      deriving (Show, Eq, Ord)

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
ioPointList :: Bool -> [Code] -> FilePath -> [FilePath] -> [IOPoint]
ioPointList stdin texts context paths =
    B.consIf stdin IOPointStdin $
         IOPointText Nothing `map` texts ++
         ioPointFrom context `map` paths


-- ----------------------  Indexed I/O point

-- | Indexed I/O point.
data IxIOPoint = IxIOPoint
    { nioNumber  :: O.Ix      -- ^ Index (0 for unindexed, > 0 for indexed)
    , nioPoint   :: IOPoint   -- ^ I/O point
    } deriving (Show)

instance Eq IxIOPoint where
    x == y  = compare x y == EQ

-- | Comapre indicies if indicies are non zero,
--   comapre I/O point if both are zero.
instance Ord IxIOPoint where
    x@IxIOPoint{ nioNumber = xn } `compare` y@IxIOPoint{ nioNumber = yn }
      | xn == 0 && yn == 0  = nioPoint x `compare` nioPoint y
      | otherwise           = xn `compare` yn

-- | Zero-numbered empty input point.
instance B.Default IxIOPoint where
    def = nioFrom Bz.empty

instance O.GetIx IxIOPoint where
    getIx = nioNumber

-- | Create input point for given lazy bytestring.
--
--   >>> nioFrom "abc"
--   IxIOPoint {nioNumber = 0, nioPoint = IOPointText Nothing "abc"}
--
nioFrom :: (ToCode code) => code -> IxIOPoint
nioFrom = IxIOPoint 0 . IOPointText Nothing . toCode

-- | This implementation uses lazy bytestring as code string.
type Code = B.Bz

-- | Convert to code string.
class ToCode a where
    toCode :: a -> Code

instance ToCode B.Bz where
    toCode = id

instance ToCode String where
    toCode = B.stringBz


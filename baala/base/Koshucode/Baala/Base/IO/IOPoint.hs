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
    ioPoint, ioPointDir, ioPointTogether,

    -- * Indexed I/O point
    IxIOPoint (..),
    codeIxIO,
    pathIxIO,
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
    = IOPointFile   FilePath FilePath    -- ^ __1 Input/Output:__ Context directory and target path.
    | IOPointUri    O.IOPath             -- ^ __2 Input:__ Universal resource identifier.
    | IOPointText   (Maybe String) Code  -- ^ __3 Input:__ Code and its name.
    | IOPointCustom String O.Bz          -- ^ __4 Input:__ Custom I/O.
    | IOPointStdin  (Maybe String)       -- ^ __5 Input:__ The sandard input.
    | IOPointStdout (Maybe String)       -- ^ __6 Output:__ The sandard output.
    | IOPointOutput NamedHandle          -- ^ __7 Output:__ Output handler.
      deriving (Show, Eq, Ord)

instance O.GetIOPath IOPoint where
    getIOPath = ioPointText

-- | Name of I/O point, i.e., @\"file\"@, @\"uri\"@, @\"text\"@,
--   @\"stdin\"@, or @\"stdout\"@.
ioPointType :: IOPoint -> String
ioPointType (IOPointFile   _ _)  = "file"
ioPointType (IOPointUri    _)    = "uri"
ioPointType (IOPointText   _ _)  = "text"
ioPointType (IOPointCustom _ _)  = "custom"
ioPointType (IOPointStdin  _)    = "stdin"
ioPointType (IOPointStdout _)    = "stdout"
ioPointType (IOPointOutput _)    = "output"

-- | Name of I/O point.
ioPointText :: IOPoint -> String
ioPointText (IOPointFile   dir file)  = dir ++ file
ioPointText (IOPointUri    uri)       = uri
ioPointText (IOPointText   name _)    = B.fromMaybe "<text>" name
ioPointText (IOPointCustom name _)    = name
ioPointText (IOPointStdin  name)      = B.fromMaybe "<stdin>" name
ioPointText (IOPointStdout name)      = B.fromMaybe "<stdout>" name
ioPointText (IOPointOutput h)         = handleName h

-- | Create I/O Point.
ioPoint :: O.IOPath -> IOPoint
ioPoint "system://stdin"    = IOPointStdin  Nothing
ioPoint "system://stdout"   = IOPointStdout Nothing
ioPoint path | isUri path   = IOPointUri  path
             | otherwise    = IOPointFile "" path

-- | Add context directory.
ioPointDir :: FilePath -> O.Map IOPoint
ioPointDir dir (IOPointFile _ path) = IOPointFile dir path
ioPointDir _ iop = iop

-- | Test path is URI.
isUri :: O.Test O.IOPath
isUri path = B.isPrefixOf "http://"  path
          || B.isPrefixOf "https://" path
          || B.isPrefixOf "ftp://"   path

-- | Create I/O points from using stdin, texts itself, filenames, and URIs.
ioPointTogether :: Bool -> [Code] -> [FilePath] -> [IOPoint]
ioPointTogether stdin texts paths =
    let ts = IOPointText Nothing <$> texts
        ps = ioPoint <$> paths
    in B.consIf stdin (IOPointStdin Nothing) $ ts ++ ps


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
    def = codeIxIO Bz.empty

instance O.GetIx IxIOPoint where
    getIx = nioNumber

instance O.GetIOPath IxIOPoint where
    getIOPath = O.getIOPath . nioPoint

-- | Create input point for given lazy bytestring.
--
--   >>> codeIxIO "abc"
--   IxIOPoint {nioNumber = 0, nioPoint = IOPointText Nothing "abc"}
--
codeIxIO :: (ToCode code) => code -> IxIOPoint
codeIxIO = IxIOPoint 0 . IOPointText Nothing . toCode

-- | Create input point from file path.
pathIxIO :: FilePath -> IxIOPoint
pathIxIO path = IxIOPoint 0 $ IOPointFile "" path

-- | This implementation uses lazy bytestring as code string.
type Code = O.Bz

-- | Convert to code string.
class ToCode a where
    toCode :: a -> Code

instance ToCode O.Bz where
    toCode = id

instance ToCode String where
    toCode = B.stringBz


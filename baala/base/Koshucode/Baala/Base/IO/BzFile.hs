{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | File path and content in lazy bytestring.

module Koshucode.Baala.Base.IO.BzFile
  ( -- * Type
    Bytes, ToBytes (..),

    -- * Read
    BzFile (..),
    readBzFile,
    tryReadFile,
  ) where

import qualified Control.Exception                as E
import qualified Data.ByteString.Lazy             as Bz
import qualified Koshucode.Baala.Overture         as O
import qualified Koshucode.Baala.Base.Abort       as B
import qualified Koshucode.Baala.Base.Prelude     as B


-- ============================================  Bytes

-- | This implementation uses lazy bytestring as input code.
type Bytes = O.Bz

-- | Convert to bytes.
class ToBytes a where
    toBytes :: a -> Bytes

instance ToBytes O.Bz where
    toBytes = id

instance ToBytes String where
    toBytes = B.stringBz


-- ============================================  Read

-- | File path and content in lazy bytestring.
--   The function 'readBzFile' creates:
--
--   * 'BzFile' with content and 'Nothing'-exception on success,
--   * 'BzFile' with empty content and 'Just'-exception on failure.
--
data BzFile = BzFile
    { bzFilePath       :: FilePath               -- ^ Path of file.
    , bzFileContent    :: B.Ab O.Bz              -- ^ File content as lazy bytestring.
    , bzFileException  :: Maybe E.SomeException  -- ^ Exception when reading file.
    } deriving (Show)

bzFileOf :: FilePath -> BzFile
bzFileOf path = BzFile { bzFilePath      = path
                       , bzFileContent   = Left notInit
                       , bzFileException = Nothing }

-- | Read file from given path.
readBzFile :: FilePath -> IO BzFile
readBzFile path =
    do content <- tryReadFile path
       let file = bzFileOf path
       return $ case content of
                  Right bz -> file { bzFileContent = Right bz }
                  Left e   -> file { bzFileContent = Left $ cannotReadFile e
                                   , bzFileException = Just e }

-- | Read file content as lazy bytestring.
tryReadFile :: (E.Exception e) => FilePath -> IO (Either e O.Bz)
tryReadFile = E.try . Bz.readFile

notInit :: B.AbortReason
notInit = B.abortBecause "Not initialized"

cannotReadFile :: (Show a) => a -> B.AbortReason
cannotReadFile e = B.abortLine "Cannot read file" $ show e

{-# OPTIONS_GHC -Wall #-}

-- | File path and content in lazy bytestring.

module Koshucode.Baala.Base.IO.BzFile
  ( BzFile (..),
    readBzFile,
    tryReadFile,
  ) where

import qualified Control.Exception             as E
import qualified Data.ByteString.Lazy          as Bz
import qualified Koshucode.Baala.Base.Prelude  as B

-- | File path and content in lazy bytestring.
--   The function 'readBzFile' creates:
--
--   * 'BzFile' with 'Just'-content and 'Nothing'-exception on success,
--   * 'BzFile' with 'Nothing'-content and 'Just'-exception on failure.

data BzFile = BzFile
    { bzFilePath       :: FilePath               -- ^ Path of file.
    , bzFileContent    :: Maybe B.Bz             -- ^ File content as lazy bytestring.
    , bzFileException  :: Maybe E.SomeException  -- ^ Exception when reading file.
    } deriving (Show)

bzFileOf :: FilePath -> BzFile
bzFileOf path = BzFile { bzFilePath      = path
                       , bzFileContent   = Nothing
                       , bzFileException = Nothing }

-- | Read file from given path.
readBzFile :: FilePath -> IO BzFile
readBzFile path =
    do content <- tryReadFile path
       let base = bzFileOf path
           file = case content of
                    Right bz -> base { bzFileContent   = Just bz }
                    Left e   -> base { bzFileException = Just e }
       return file

-- | Read file content as lazy bytestring.
tryReadFile :: (E.Exception e) => FilePath -> IO (Either e B.Bz)
tryReadFile = E.try . Bz.readFile


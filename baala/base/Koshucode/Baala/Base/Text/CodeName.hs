{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.CodeName
  ( -- * Name of code
    CodeName (..),
    codeNameType, codeNameText,
    codeNameFrom, codeNameList,

    -- * Code information
    CodePiece (..),
    codeEmpty, codeTextOf,
  ) where

import qualified Data.Generics                as G
import qualified Koshucode.Baala.Base.Prelude as B


-- ----------------------  CodeName

data CodeName
    = CodeFile  FilePath
    | CodeUri   String
    | CodeText  String
    | CodeStdin
    | CodeStdout
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Name of code type, i.e., @\"file\"@, @\"url\"@, @\"text\"@,
--   @\"stdin\"@, or @\"stdout\"@.
codeNameType :: CodeName -> String
codeNameType (CodeFile _)     = "file"
codeNameType (CodeUri  _)     = "url"
codeNameType (CodeText _)     = "text"
codeNameType (CodeStdin)      = "stdin"
codeNameType (CodeStdout)     = "stdout"

codeNameText :: CodeName -> String
codeNameText (CodeFile file)  = file
codeNameText (CodeUri  url)   = url
codeNameText (CodeText text)  = text
codeNameText (CodeStdin)      = "<stdin>"
codeNameText (CodeStdout)     = "<stdout>"

codeNameFrom :: String -> CodeName
codeNameFrom path
    | B.isPrefixOf "http://"  path  = CodeUri  path
    | B.isPrefixOf "https://" path  = CodeUri  path
    | B.isPrefixOf "ftp://"   path  = CodeUri  path
    | otherwise                     = CodeFile path

-- | Create sources from using stdin, texts itself, filenames, and urls.
codeNameList :: Bool -> [String] -> [String] -> [CodeName]
codeNameList stdin texts paths =
    B.consIf stdin CodeStdin $
         CodeText     `map` texts ++
         codeNameFrom `map` paths


-- ----------------------  CodePiece

data CodePiece
    = CodePiece { codeNumber :: Int
                , codeName   :: CodeName }
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


{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Source
  ( Source (..), SourceName (..),
    sourceType, sourceText, sourceNameText,
    sourceZero, sourceFrom, sourceNameFrom,
    sourceOf, sourceList, sourceNameList,
  ) where

import qualified Data.Generics                as G
import qualified Koshucode.Baala.Base.Prelude as B

data Source
    = Source { sourceNumber :: Int
             , sourceName   :: SourceName }
      deriving (Show, G.Data, G.Typeable)

instance Eq Source where
    x == y = sourceName x == sourceName y

instance Ord Source where
    x `compare` y = sourceName x `compare` sourceName y

data SourceName
    = SourceFile  FilePath
    | SourceURL   String
    | SourceText  String
    | SourceStdin
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Name of resourcd type, i.e., @\"file\"@, @\"text\"@, @\"url\"@.
sourceType :: Source -> String
sourceType = sourceNameType . sourceName

sourceText :: Source -> String
sourceText = sourceNameText . sourceName

sourceNameType :: SourceName -> String
sourceNameType (SourceFile _)     = "file"
sourceNameType (SourceURL  _)     = "url"
sourceNameType (SourceText _)     = "text"
sourceNameType (SourceStdin)      = "stdin"

sourceNameText :: SourceName -> String
sourceNameText (SourceFile file)  = file
sourceNameText (SourceURL  url)   = url
sourceNameText (SourceText text)  = text
sourceNameText (SourceStdin)      = "<stdin>"

-- | Empty source.
sourceZero :: Source
sourceZero = sourceOf ""

-- | Create text source.
sourceOf :: String -> Source
sourceOf = Source 0 . SourceText

sourceFrom :: String -> Source
sourceFrom = Source 0 . sourceNameFrom

sourceNameFrom :: String -> SourceName
sourceNameFrom path
    | B.isPrefixOf "http://"  path  = SourceURL  path
    | B.isPrefixOf "https://" path  = SourceURL  path
    | B.isPrefixOf "ftp://"   path  = SourceURL  path
    | otherwise                     = SourceFile path

sourceStdin :: Source
sourceStdin = Source 0 SourceStdin

-- | Create sources from using stdin, texts itself, filenames, and urls.
sourceList :: Bool -> [String] -> [String] -> [Source]
sourceList stdin texts paths =
    B.consIf stdin sourceStdin $
         sourceOf   `map` texts ++
         sourceFrom `map` paths

sourceNameList :: Bool -> [String] -> [String] -> [SourceName]
sourceNameList stdin texts paths =
    B.consIf stdin SourceStdin $
         SourceText     `map` texts ++
         sourceNameFrom `map` paths


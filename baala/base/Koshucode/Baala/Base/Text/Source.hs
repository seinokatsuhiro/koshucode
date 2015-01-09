{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Source
  ( Source (..), SourceName (..),
    sourceType, sourceText,
    sourceZero, sourceOf, sourceList,
  ) where

import qualified Data.Generics as G

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

-- | Create sources from using stdin, texts itself, filenames, and urls.
sourceList :: Bool -> [String] -> [String] -> [String] -> [Source]
sourceList stdin texts files urls = zipWith Source [1..] names where
    input = if stdin then [SourceStdin] else []
    names = input ++
            SourceText `map` texts ++
            SourceFile `map` files ++
            SourceURL  `map` urls


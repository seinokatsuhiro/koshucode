{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Source
  (
    -- * Source
    Source (..),
    SourceName (..),
    sourceType,
    sourceText,
    sourceZero,
    sourceOf,
    sourceList,
  
    -- * Code point
    CodePt (..),
    codeZero,
    codeColumnNumber,
    codeDisplay,
  
    -- * Code pointer
    CodePtr (..),
  
    -- * Sourced
    Sourced (..),
  ) where

import qualified Data.Generics                 as G
import qualified Koshucode.Baala.Base.Prelude  as B


-- ----------------------  Source

data Source
    = Source { sourceNumber :: Int
             , sourceName   :: SourceName }
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

data SourceName
    = SourceFile  String
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
sourceNameText (SourceFile file)  =  file
sourceNameText (SourceURL  url)   =  url
sourceNameText (SourceText text)  =  text
sourceNameText (SourceStdin)      =  "<stdin>"

sourceZero :: Source
sourceZero = sourceOf ""

sourceOf :: String -> Source
sourceOf = Source 0 . SourceText

sourceList :: Bool -> [String] -> [String] -> [String] -> [Source]
sourceList stdin texts files urls = zipWith Source [1..] names where
    input = if stdin then [SourceStdin] else []
    names = input ++
            SourceText `map` texts ++
            SourceFile `map` files ++
            SourceURL  `map` urls


-- ----------------------  CodePt

data CodePt = CodePt
      { codeSource     :: Source      -- ^ Source of code
      , codeLineNumber :: Int         -- ^ Line number
      , codeLineText   :: String      -- ^ Line content
      , codeText       :: String      -- ^ Text at which begins token
      } deriving (Show, Eq, G.Data, G.Typeable)

instance Ord CodePt where
    compare = codeCompare

codeCompare :: CodePt -> CodePt -> Ordering
codeCompare p1 p2 = line `B.mappend` column where
    line   = codeLineNumber p1 `compare` codeLineNumber p2
    column = size p2 `compare` size p1
    size   = length . codeText

-- | Empty code point, i.e., empty content and zero line number.
codeZero :: CodePt
codeZero = CodePt sourceZero 0 "" ""

-- | Column number at which code starts.
codeColumnNumber :: CodePt -> Int
codeColumnNumber CodePt { codeLineText = line, codeText = subline }
    = length line - length subline

codeDisplay :: (String, CodePt) -> [(String, String)]
codeDisplay (tag, p)
    | lno > 0   = [ (pos, ""), ("> " ++ shorten text, tag) ]
    | otherwise = []
    where
      pos  = show lno ++ " " ++ show cno ++ " " ++ res
      lno  = codeLineNumber p
      cno  = codeColumnNumber p
      res  = sourceText $ codeSource p
      text = codeText p

      shorten :: B.Map String
      shorten s | length s > 48 = take 45 s ++ "..."
                | otherwise     = s


-- ----------------------  CodePtr

class CodePtr a where
    codePts :: a -> [CodePt]

    codePt :: a ->  CodePt
    codePt p = B.headOr codeZero $ codePts p

instance CodePtr CodePt where
    codePts cp  = [cp]
    codePt  cp  =  cp


-- ----------------------  Sourced

data Sourced a =
    Sourced { source    :: [CodePt]
            , unsourced :: a
            } deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance Functor Sourced where
    fmap f (Sourced src x) = Sourced src $ f x

instance CodePtr (Sourced a) where
    codePts = source

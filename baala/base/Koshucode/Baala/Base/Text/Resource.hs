{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Resource
(
  -- * Resource
  Resource (..),
  resourceType,
  resourceName,

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


-- ----------------------  Resource

data Resource
    = ResourceFile String
    | ResourceText String
    | ResourceURL  String
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Name of resourcd type, i.e., @\"file\"@, @\"text\"@, @\"url\"@.
resourceType :: Resource -> String
resourceType (ResourceFile _)     = "file"
resourceType (ResourceText _)     = "text"
resourceType (ResourceURL  _)     = "url"

resourceName :: Resource -> String
resourceName (ResourceFile file)  =  file
resourceName (ResourceText text)  =  text
resourceName (ResourceURL  url)   =  url


-- ----------------------  CodePt

data CodePt = CodePt
      { codeResource   :: Resource    -- ^ Resource of code
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
codeZero = CodePt (ResourceText "") 0 "" ""

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
      res  = resourceName $ codeResource p
      text = codeText p

      shorten :: B.Map String
      shorten s | length s > 48 = take 45 s ++ "..."
                | otherwise     = s


-- ----------------------  CodePtr

class CodePtr a where
    codePts :: a -> [CodePt]

instance CodePtr CodePt where
    codePts pt = [pt]


-- ----------------------  Sourced

data Sourced a =
    Sourced { source    :: [CodePt]
            , unsourced :: a
            } deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance Functor Sourced where
    fmap f (Sourced src x) = Sourced src $ f x

instance CodePtr (Sourced a) where
    codePts = source

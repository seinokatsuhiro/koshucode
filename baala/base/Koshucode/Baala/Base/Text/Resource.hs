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
  codePointZero,
  codePointColumnNumber,
  codePointDisplay,

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
      { codePointResource   :: Resource    -- ^ Resource of code
      , codePointLineNumber :: Int         -- ^ Line number
      , codePointLineText   :: String      -- ^ Line content
      , codePointText       :: String      -- ^ Text at which begins token
      } deriving (Show, Eq, G.Data, G.Typeable)

instance Ord CodePt where
    compare = codePointCompare

codePointCompare :: CodePt -> CodePt -> Ordering
codePointCompare p1 p2 = line `B.mappend` column where
    line   = codePointLineNumber p1 `compare` codePointLineNumber p2
    column = size p2 `compare` size p1
    size   = length . codePointText

-- | Empty code point, i.e., empty content and zero line number.
codePointZero :: CodePt
codePointZero = CodePt (ResourceText "") 0 "" ""

-- | Column number at which code starts.
codePointColumnNumber :: CodePt -> Int
codePointColumnNumber CodePt { codePointLineText = line, codePointText = subline }
    = length line - length subline

codePointDisplay :: (String, CodePt) -> [(String, String)]
codePointDisplay (tag, p)
    | lno > 0   = [ (pos, ""), ("> " ++ shorten text, tag) ]
    | otherwise = []
    where
      pos  = show lno ++ " " ++ show cno ++ " " ++ res
      lno  = codePointLineNumber p
      cno  = codePointColumnNumber p
      res  = resourceName $ codePointResource p
      text = codePointText p

      shorten :: B.Map String
      shorten s | length s > 48 = take 45 s ++ "..."
                | otherwise     = s


-- ----------------------  CodePtr

class CodePtr a where
    codePoints :: a -> [CodePt]

instance CodePtr CodePt where
    codePoints pt = [pt]


-- ----------------------  Sourced

data Sourced a =
    Sourced { source    :: [CodePt]
            , unsourced :: a
            } deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance Functor Sourced where
    fmap f (Sourced src x) = Sourced src $ f x

instance CodePtr (Sourced a) where
    codePoints = source

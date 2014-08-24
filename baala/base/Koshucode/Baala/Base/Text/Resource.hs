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
  codePtZero,
  codePtColumnNumber,
  codePtDisplay,

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
      { codePtResource   :: Resource    -- ^ Resource of code
      , codePtLineNumber :: Int         -- ^ Line number
      , codePtLineText   :: String      -- ^ Line content
      , codePtText       :: String      -- ^ Text at which begins token
      } deriving (Show, Eq, G.Data, G.Typeable)

instance Ord CodePt where
    compare = codePtCompare

codePtCompare :: CodePt -> CodePt -> Ordering
codePtCompare p1 p2 = line `B.mappend` column where
    line   = codePtLineNumber p1 `compare` codePtLineNumber p2
    column = size p2 `compare` size p1
    size   = length . codePtText

-- | Empty code point, i.e., empty content and zero line number.
codePtZero :: CodePt
codePtZero = CodePt (ResourceText "") 0 "" ""

-- | Column number at which code starts.
codePtColumnNumber :: CodePt -> Int
codePtColumnNumber CodePt { codePtLineText = line, codePtText = subline }
    = length line - length subline

codePtDisplay :: (String, CodePt) -> [(String, String)]
codePtDisplay (tag, p)
    | lno > 0   = [ (pos, ""), ("> " ++ shorten text, tag) ]
    | otherwise = []
    where
      pos  = show lno ++ " " ++ show cno ++ " " ++ res
      lno  = codePtLineNumber p
      cno  = codePtColumnNumber p
      res  = resourceName $ codePtResource p
      text = codePtText p

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

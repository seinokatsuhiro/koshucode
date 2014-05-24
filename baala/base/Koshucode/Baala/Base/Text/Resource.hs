{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Resource
(
  -- * Resource
  Resource (..),
  resourceType,
  resourceName,

  -- * CodePoint
  CodePoint (..),
  CodePointer (..),
  codePointColumn,
  codePointDisplay,
  codePointZero,

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

resourceType :: Resource -> String
resourceType (ResourceFile _)     = "file"
resourceType (ResourceText _)     = "text"
resourceType (ResourceURL  _)     = "url"

resourceName :: Resource -> String
resourceName (ResourceFile file)  =  file
resourceName (ResourceText text)  =  text
resourceName (ResourceURL  url)   =  url


-- ----------------------  CodePoint

data CodePoint = CodePoint
      { codePointResource   :: Resource    -- ^ Resource of code
      , codePointLineNumber :: Int         -- ^ Line number
      , codePointLineText   :: String      -- ^ Line content
      , codePointText       :: String      -- ^ Text at which begins token
      } deriving (Show, Eq, G.Data, G.Typeable)

instance Ord CodePoint where
    compare p1 p2
        = (codePointLineNumber p1 `compare` codePointLineNumber p2)
          `B.mappend` (codePointTextLength p2 `compare` codePointTextLength p1)

class CodePointer a where
    codePoint :: a -> [CodePoint]

instance CodePointer CodePoint where
    codePoint pt = [pt]

codePointTextLength :: CodePoint -> Int
codePointTextLength = length . codePointText

codePointColumn :: CodePoint -> Int
codePointColumn CodePoint { codePointLineText = line, codePointText = subline }
    = length line - length subline

codePointDisplay :: String -> CodePoint -> [(String, String)]
codePointDisplay tag p
    | lno > 0   = [(pos, ""), ("> " ++ shorten text, tag)]
    | otherwise = []
    where
      pos  = show lno ++ " " ++ show cno ++ " " ++ res
      lno  = codePointLineNumber p
      cno  = codePointColumn p
      res  = resourceName $ codePointResource p
      text = codePointText p

codePointZero :: CodePoint
codePointZero = CodePoint (ResourceText "") 0 "" ""

shorten :: B.Map String
shorten s | length s > 48 = take 45 s ++ "..."
          | otherwise     = s


-- ----------------------  Sourced

data Sourced a =
    Sourced { source    :: [CodePoint]
            , unsourced :: a
            } deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance Functor Sourced where
    fmap f (Sourced src x) = Sourced src $ f x

instance CodePointer (Sourced a) where
    codePoint = source

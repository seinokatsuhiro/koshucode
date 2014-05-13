{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Token.Resource
(
  -- * Resource
  Resource (..),
  resourceType,
  resourceName,

  -- * CodePoint
  CodePoint (..),
  CodePointer (..),
  codePointLineNumber,
  codePointLineText,
  codePointColumn,
  codePointDisplay,
  codePointZero,

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
      { codePointResource :: Resource
      , codePointLine     :: (Int, String)   -- ^ Line number and content
      , codePointText     :: String          -- ^ Text at which begins token
      } deriving (Show, Eq, G.Data, G.Typeable)

instance Ord CodePoint where
    compare p1 p2
        = (codePointLineNumber p1 `compare` codePointLineNumber p2)
          `B.mappend` (codePointTextLength p2 `compare` codePointTextLength p1)

class CodePointer a where
    codePoint :: a -> CodePoint

codePointLineNumber :: CodePoint -> Int
codePointLineNumber = fst . codePointLine

codePointLineText   :: CodePoint -> String
codePointLineText   = snd . codePointLine

codePointTextLength :: CodePoint -> Int
codePointTextLength = length . codePointText

codePointColumn :: CodePoint -> Int
codePointColumn CodePoint { codePointLine = (_, line), codePointText = subline }
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
codePointZero = CodePoint (ResourceText "") (0, "") ""

shorten :: B.Map String
shorten s | length s > 48 = take 45 s ++ "..."
          | otherwise     = s


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Syntax.HashWord
(
  -- * Library
  hashWordTable,
  hashWord,
  hashWordInvert,
  hashString,
  hashSplit,

  -- * Document
  -- $HashWord

  -- * Example
  -- $HashSplit
) where

import qualified Data.Tuple as D

hashWordTable :: [(String, String)]
hashWordTable =
    [ o  "q"     "'"
    , o  "qq"    "\""
    , o  "cr"    "\r"
    , o  "lf"    "\n"
    , o  "crlf"  "\r\n"
    , o  "tab"   "\t"
    , o  "sp"    " "
    , o  "("     "("
    , o  ")"     ")"
    , o  "{"     "{"
    , o  "}"     "}"
    , o  "["     "["
    , o  "]"     "]"
    , o  ""      "#"
    ] where o = (,)

{-|  -}
hashWord :: String -> Maybe String
hashWord w = lookup w hashWordTable

hashWordInvert :: String -> Maybe String
hashWordInvert w = lookup w tab
    where tab = map D.swap hashWordTable

hashString :: String -> String
hashString = unwords . hashSplit

hashSplit :: String -> [String]
hashSplit = non False [] where
    hash [] = []
    hash (c : cs) =
        case hashWordInvert [c] of
          Nothing -> non False [c] cs
          Just h  -> ('#' : h) : hash cs

    non q s [] = [rev q s]
    non _ s (' ' : cs) = non True (' ' : s) cs
    non q s (c : cs) =
        case hashWordInvert [c] of
          Nothing -> non q (c : s) cs
          Just h  -> rev q s : ('#' : h) : hash cs

    rev :: Bool -> String -> String
    rev False s = reverse s
    rev True  s = "'" ++ reverse s ++ "'"



-- ----------------------
{- $HashWord

   (Not implemented)
   
   [@\[#q\]@]
    Single quote.
   
   [@\[#qq\]@]
    Double quote.
   
   [@\[#cr\]@]
    Carriage return (@\\r@).
   
   [@\[#lf\]@]
    Line feed (@\\n@).
   
   [@\[#crlf\]@]
    Carriage return and  line feed (@\\r\\n@).
   
   [@\[#tab\]@]
    Tab (@\\t@).
   
   [@\[#spc\]@]
    Space.

   [@\[#\]@]
    Hash mark itself.
   -}


-- ----------------------
{- $HashSplit

   hashSplit

   >>> hashSplit "aaa"
   ["aaa"]

   >>> hashSplit "aaa bbb"
   ["'aaa bbb'"]

   >>> hashSplit "aaa'bbb"
   ["aaa", "#q", "bbb"]

   >>> hashSplit "aaa\nbbb"
   ["aaa", "#lf", "bbb"]

-}


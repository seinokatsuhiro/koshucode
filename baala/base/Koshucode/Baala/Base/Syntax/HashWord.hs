{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Syntax.HashWord
(
  -- * Library
  hashWordTable,
  hashWord,
  hashWordInvert,
  hashSplit,

  -- * Document
  -- $HashWord

  -- * Example
  -- $HashSplit
) where

import qualified Data.Char as Ch
import qualified Koshucode.Baala.Base.Prelude as B

hashWordTable :: [(String, String)]
hashWordTable =
    [ o  "q"     "'"
    , o  "qq"    "\""
    , o  "cr"    "\r"
    , o  "lf"    "\n"
    , o  "crlf"  "\r\n"
    , o  "tab"   "\t"
    , o  "sp"    " "
    , o  "empty" ""
    , o  "("     "("
    , o  ")"     ")"
    , o  "{"     "{"
    , o  "}"     "}"
    , o  "["     "["
    , o  "]"     "]"
    , o  "/"     "/"
    , o  ""      "#"
    ] where o = (,)

{-|  -}
hashWord :: String -> Maybe String
hashWord w = lookup w hashWordTable

-- hashWordInvert :: String -> Maybe String
-- hashWordInvert w = lookup w tab
--     where tab = map D.swap hashWordTable

hashWordInvert :: String -> Maybe (String, String)
hashWordInvert [] =  Nothing
hashWordInvert (c : cs)
    = case B.generalCategoryGroup c of
        B.UnicodePunctuation  -> punct c
        B.UnicodeOther        -> other c
        _                     -> Nothing
      where
        punct '\'' = Just ("#q", cs)
        punct '"'  = Just ("#qq", cs)
        punct _    = Nothing

        other '\t' = Just ("#tab", cs)
        other '\r' = case cs of
                       ('\n' : cs2) -> Just ("#crlf", cs2)
                       _ -> Just ("#cr", cs)
        other '\n' = Just ("#lf", cs)
        other _    = Just ("#n" ++ show code, cs)

        code = Ch.ord c

hashSplit :: String -> [String]
hashSplit = non "" where
    non s "" = [rev s]
    non s (' ' : cs) = non (' ' : s) cs
    non s ccs@(c : cs) =
        case hashWordInvert ccs of
          Nothing       -> non (c : s) cs
          Just (h, cs2) -> let hashed = h : hash cs2
                           in if s == ""
                              then hashed
                              else rev s : hashed

    hash [] = []
    hash ccs@(c : cs) =
        case hashWordInvert ccs of
          Nothing       -> non [c] cs
          Just (h, cs2) -> h : hash cs2

    rev :: String -> String
    rev s  = qq : (reverse $ qq : s)

    qq = '"'



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


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Content.HashWord
( -- $HashWord
  hashWordTable,
  hashWord,
) where

import qualified Data.Char as Ch
import qualified Koshucode.Baala.Base.Prelude as B

infixr 0 -:-
(-:-) :: a -> b -> (a, b)
(-:-) = (,)

{-| Table of coresspondences of hashed sequence and its text.

    >>> lookup "lf" hashWordTable
    Just "\n"  -}
hashWordTable :: [(String, String)]
hashWordTable =
    [ "cr"     -:-  "\r"
    , "crlf"   -:-  "\r\n"
    , "empty"  -:-  ""
    , "lf"     -:-  "\n"
    , "q"      -:-  "'"
    , "qq"     -:-  "\""
    , "sp"     -:-  " "
    , "tab"    -:-  "\t"
    ]

{-| Convert string into hashed form.

    >>> putStrLn $ hashWord "aaa"
    "aaa"

    >>> putStrLn $ hashWord "aaa\nbbb"
    "aaa" #lf "bbb"
-}
hashWord :: B.Map String
hashWord = open . loop where
    loop "" = "\""
    loop ccs@(c : cs) =
        case hashSplit ccs of
          Nothing       -> c : loop cs
          Just (h, cs2) -> "\" " ++ h ++ sp (open $ loop cs2)

    open ('"' : cs) = trim cs
    open cs         = '"' : cs

    sp ""           = ""
    sp cs           = ' ' : cs

    trim (' ' : cs) = trim cs
    trim cs         = cs

{-| >>> hashSplit "abc"
    Nothing

    >>> hashSplit "\nabc"
    Just ("#lf", "abc")  -}
hashSplit :: String -> Maybe (String, String)
hashSplit [] = Nothing
hashSplit (c : cs)
    = case B.generalCategoryGroup c of
        B.UnicodePunctuation -> punct c
        B.UnicodeOther       -> other c
        _                    -> Nothing
      where
        punct '\''      = Just ("#q", cs)
        punct '"'       = Just ("#qq", cs)
        punct _         = Nothing

        other '\r'      = cr cs
        other '\n'      = Just ("#lf", cs)
        other '\t'      = Just ("#tab", cs)
        other _         = Just ("#u" ++ show code, cs)

        cr ('\n' : cs2) = Just ("#crlf", cs2)
        cr _            = Just ("#cr", cs)

        code            = Ch.ord c


-- -------------------------------------------------------
{- $HashWord

   Text literals are written
   using the following hashed words.

   [@#q@]      Single quote.

   [@#qq@]     Double quote.

   [@#tab@]    Tab (@\\t@).

   [@#sp@]     Space.

   [@#cr@]     Carriage return (@\\r@).

   [@#lf@]     Line feed (@\\n@).

   [@#crlf@]   Carriage return and  line feed (@\\r\\n@).

-}


{-# OPTIONS_GHC -Wall #-}

-- | Text enclosed in angle brackets.

module Koshucode.Baala.Syntax.Symbol.AngleText
  ( -- * Functions
    angleQuote,
    angleTexts,
    -- * Keyword table
    -- $Table
  ) where

import qualified Data.Char                    as Ch
import qualified Koshucode.Baala.Overture     as O

-- | Convert string into double-quoted and angle-quoted form.
--
--   >>> putStrLn $ angleQuote "aaa"
--   "aaa"
--
--   >>> putStrLn $ angleQuote "\t"
--   <tab>
--
--   >>> putStrLn $ angleQuote "aaa\nbbb\r\nccc\r\n\r\nddd"
--   "aaa" <lf> "bbb" <crlf> "ccc" <crlf> <crlf> "ddd"
--
angleQuote :: O.Map String
angleQuote = open . loop where
    loop "" = "\""
    loop ccs@(c : cs) =
        case angleSplit ccs of
          Nothing       -> c : loop cs
          Just (w, cs2) -> "\" " ++ w ++
                           O.addSpace (open $ loop cs2)

    open ('"' : cs)  = O.trimLeft cs   -- omit closing double quote
    open cs          = '"' : cs        -- append opening double quote

-- | Split angle text.
--
--   >>> angleSplit "abc"
--   Nothing
--
--   >>> angleSplit "\n\nabc"
--   Just ("<lf>", "abc")
--
angleSplit :: String -> Maybe (String, String)
angleSplit [] = Nothing
angleSplit (c : cs)
    = case O.majorGeneralCategory c of
        O.UnicodePunctuation -> punct c
        O.UnicodeOther       -> other c
        _                    -> Nothing
      where
        just cs2 a           =  Just (a, cs2)

        punct '"'            =  just cs "<qq>"
        punct _              =  Nothing

        other '\r'           =  cr cs
        other '\n'           =  just cs "<lf>"
        other '\t'           =  just cs "<tab>"
        other _              =  just cs $ "<c" ++ show code ++ ">"

        cr ('\n' : cs2)      =  just cs2 "<crlf>"
        cr _                 =  just cs  "<cr>"

        code                 =  Ch.ord c

-- | Table of coresspondences of angle text and its replacement.
--
--   >>> lookup "lf" angleTexts
--   Just "\n"
--
angleTexts :: [(String, String)]
angleTexts =
    --  NAME         REPLACEMENT
    [ o "cr"         "\r"
    , o "crlf"       "\r\n"
    , o "lf"         "\n"
    , o "q"          "'"
    , o "qq"         "\""
    , o "sp"         " "
    , o "tab"        "\t"
    , o "dot"        "."
    , o "comma"      ","
    , o "colon"      ":"
    , o "semicolon"  ";"
    ] where o = (,)

-- $Table
--
--  Some text literals are written
--  using the following angle text.
--
--  [@\<cr\>@]      Carriage return
--
--  [@\<crlf\>@]    Carriage return and line feed
--
--  [@\<lf\>@]      Line feed
--
--  [@\<q\>@]       Single quote (@'@)
--
--  [@\<qq\>@]      Double quote (@\"@)
--
--  [@\<sp\>@]      Space
--
--  [@\<tab\>@]     Tab
--


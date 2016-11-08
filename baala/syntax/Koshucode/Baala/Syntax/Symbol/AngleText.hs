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
angleQuote = openLoop where
    openLoop = open . loop

    loop (c : cs) = case angleSplit c cs of
                      Nothing       -> c : loop cs
                      Just (w, cs2) -> "\" " ++ w ++
                                       O.addSpace (openLoop cs2)
    loop "" = "\""

    open ('"' : cs)  = O.trimBegin cs   -- omit closing double quote
    open cs          = '"' : cs         -- append opening double quote

-- | Split angle keyword.
--
--   >>> let (c : cs) = "abc" in angleSplit c cs
--   Nothing
--
--   >>> let (c : cs) =  "\nabc" in angleSplit c cs
--   Just ("<lf>", "abc")
--
--   >>> let (c : cs) =  "\0abc" in angleSplit c cs
--   Just ("<c0>", "abc")
--
angleSplit :: Char -> String -> Maybe (String, String)
angleSplit c cs =
    case O.majorGeneralCategory c of
      O.UnicodePunctuation -> punct c
      O.UnicodeOther       -> other c
      _                    -> Nothing
    where
      just w               = Just (w, cs)
      just2 w cs2          = Just (w, cs2)

      punct '"'            = just "<qq>"
      punct _              = Nothing

      other '\r'           = cr cs
      other '\n'           = just "<lf>"
      other '\t'           = just "<tab>"
      other _              = just (angleChar c)

      cr ('\n' : cs2)      = just2 "<crlf>" cs2
      cr _                 = just "<cr>"

-- | Angle text of char code.
--
--   >>> angleChar 'a'
--   "<c97>"
--
angleChar :: Char -> String
angleChar c = "<c" ++ show (Ch.ord c) ++ ">"

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


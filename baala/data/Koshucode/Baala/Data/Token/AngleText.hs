{-# OPTIONS_GHC -Wall #-}

-- | Text enclosed in angle brackets.

module Koshucode.Baala.Data.Token.AngleText
  ( 
    -- ** Functions
    angleQuote,
    angleTexts,
  
    -- ** Examples
    -- $Angle
  
    -- ** Keyword table
    -- $Table
  ) where

import qualified Data.Char                    as Ch
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Text    as B

-- $Angle
--
--  Simple word.
--
--    >>> putStrLn $ angleQuote "aaa"
--    "aaa"
--
--  Text contains line feed.
--
--    >>> putStrLn $ angleQuote "aaa\nbbb"
--    "aaa" #lf "bbb"
--
--  Lookup keyword table.
--
--    >>> lookup "lf" angleTexts
--    Just "\n"
--

-- | Convert string into double-quoted and angle-quoted form.

angleQuote :: B.Map String
angleQuote = open . loop where
    loop "" = "\""
    loop ccs@(c : cs) =
        case angleSplit ccs of
          Nothing       -> c : loop cs
          Just (w, cs2) -> "\" " ++ w ++ sp (open $ loop cs2)

    open ('"' : cs) = trim cs
    open cs         = '"' : cs

    sp ""           = ""
    sp cs           = ' ' : cs

    trim (' ' : cs) = trim cs
    trim cs         = cs

--  >>> angleSplit "abc"
--  Nothing
--
--  >>> angleSplit "\nabc"
--  Just ("<lf>", "abc")

angleSplit :: String -> Maybe (String, String)
angleSplit [] = Nothing
angleSplit (c : cs)
    = case B.generalCategoryGroup c of
        B.UnicodePunctuation -> punct c
        B.UnicodeOther       -> other c
        _                    -> Nothing
      where
        just cs2 a           =  Just ("<" ++ a ++ ">", cs2)

        punct '"'            =  just cs "qq"
        punct _              =  Nothing

        other '\r'           =  cr cs
        other '\n'           =  just cs "lf"
        other '\t'           =  just cs "tab"
        other _              =  just cs $ "c" ++ show code

        cr ('\n' : cs2)      =  just cs2 "crlf"
        cr _                 =  just cs  "cr"

        code                 =  Ch.ord c

-- | Table of coresspondences of angle text and its replacement.

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


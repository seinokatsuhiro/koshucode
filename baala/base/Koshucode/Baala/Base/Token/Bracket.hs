{-# OPTIONS_GHC -Wall #-}

-- | Bracket keywords.

module Koshucode.Baala.Base.Token.Bracket
( bracketQuote,
  bracketTable,
  -- $Bracket

  -- ** Table
  -- $Table
) where

import qualified Data.Char                    as Ch
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Text    as B


-- ------------------------------------------------------------------
-- $Bracket
--
--  /Examples/
--
--  >>> putStrLn $ bracketQuote "aaa"
--  "aaa"
--
--  >>> putStrLn $ bracketQuote "aaa\nbbb"
--  "aaa" #lf "bbb"
--
--  >>> lookup "lf" bracketTable
--  Just "\n"
--

-- | Convert string into double-quoted and bracket-quoted form.
bracketQuote :: B.Map String
bracketQuote = open . loop where
    loop "" = "\""
    loop ccs@(c : cs) =
        case bracketSplit ccs of
          Nothing       -> c : loop cs
          Just (w, cs2) -> "\" " ++ w ++ sp (open $ loop cs2)

    open ('"' : cs) = trim cs
    open cs         = '"' : cs

    sp ""           = ""
    sp cs           = ' ' : cs

    trim (' ' : cs) = trim cs
    trim cs         = cs

-- | >>> bracketSplit "abc"
--   Nothing
--   >>> bracketSplit "\nabc"
--   Just ("<lf>", "abc")
bracketSplit :: String -> Maybe (String, String)
bracketSplit [] = Nothing
bracketSplit (c : cs)
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

-- | Table of coresspondences of bracket keyword and its text.
bracketTable :: [(String, String)]
bracketTable =
    --  KEYWORD   REPLACEMENT
    [ o "cr"      "\r"
    , o "crlf"    "\r\n"
    , o "empty"   ""
    , o "lf"      "\n"
    , o "q"       "'"
    , o "qq"      "\""
    , o "sp"      " "
    , o "tab"     "\t"
    ] where o = (,)


-- -------------------------------------------------------
-- $Table
--
--  Some text literals are written
--  using the following bracket keywords.
--
--  [@\<cr\>@]      Carriage return. (@\\r@)
--
--  [@\<crlf\>@]    Carriage return and  line feed. (@\\r\\n@)
--
--  [@\<empty\>@]   Empty string.
--
--  [@\<lf\>@]      Line feed. (@\\n@)
--
--  [@\<q\>@]       Single quote. (@'@)
--
--  [@\<qq\>@]      Double quote. (@\"@)
--
--  [@\<sp\>@]      Space.
--
--  [@\<tab\>@]     Tab. (@\\t@)
--


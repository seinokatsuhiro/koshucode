{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Text enclosed in angle brackets.

module Koshucode.Baala.Syntax.Symbol.AngleText
  ( -- * Functions
    angleQuote,
    angleTexts,
    -- * Keyword table
    -- $Table
  ) where

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
angleQuote :: (O.Textual t) => t -> t
angleQuote = O.tJoinWith " " . angleList

{-| Divdie text into angle (Left) and non-angle (Right) subtexts.

    >>> angleList "foo\r\nbar\n\n\"baz\""
    ["\"foo\"", "<crlf>", "\"bar\"", "<lf>", "<lf>", "<qq>", "\"baz\"", "<qq>"]
    -}
angleList :: (O.Textual t) => t -> [t]
angleList = loop where
    loop t | O.tIsEmpty t = []
           | otherwise    = case O.span nonAngle t of
                              (a, b) | O.tIsEmpty b -> [non a]
                                     | O.tIsEmpty a ->         angle b
                                     | otherwise    -> non a : angle b

    non t = '"' O.<:> (t O.++ "\"")

    angle (O.cut -> O.Jp c t)
        = case O.majorGeneralCategory c of
            O.UnicodePunctuation -> punct c t
            O.UnicodeOther       -> other c t
            _                    -> char c t
    angle _ = []

    punct '"' t                = "<qq>" : loop t
    punct c   t                = char c t

    other '\r' t               = cr t
    other '\n' t               = "<lf>"  : loop t
    other '\t' t               = "<tab>" : loop t
    other c    t               = char c t

    cr (O.cut -> O.Jp '\n' t)  = "<crlf>" : loop t
    cr t                       = "<cr>"   : loop t

    char c t                   = angleChar c : loop t

{-| Test non-angle character.

    >>> nonAngle <$> "foo\r\nbar\"baz"
    [True,True,True,False,False,True,True,True,False,True,True,True]
    -}
nonAngle :: Char -> Bool
nonAngle c =
    case O.majorGeneralCategory c of
      O.UnicodePunctuation -> c /= '"'
      O.UnicodeOther       -> False
      _                    -> True

-- | Angle text of the Unicode code point.
--
--   >>> angleChar 'K' :: String
--   "<U+4B>"
--
angleChar :: (O.Textual t) => Char -> t
angleChar c = "<U+" O.++ O.intUpperHexString (fromEnum c) O.++ ">"

-- | Table of coresspondences of angle text and its replacement.
--
--   >>> lookup "lf" angleTexts
--   Just "\n"
--
angleTexts :: (O.Textual t) => [(t, t)]
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


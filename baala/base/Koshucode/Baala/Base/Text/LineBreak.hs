{-# OPTIONS_GHC -Wall #-}

-- | Line break setting for mix text.

module Koshucode.Baala.Base.Text.LineBreak
  ( -- * Line break
    LineBreak (..),
    lfString, crlfString,

    -- * Unlimited line width
    noBreak, lfBreak, crlfBreak,

    -- * Limited line width
    lf2, lf4, lf8,
    crlf2, crlf4, crlf8,
  ) where

import qualified Data.Default as Def

-- | Line break setting.
data LineBreak = LineBreak
    { breakWidth     :: Maybe Int  -- ^ Max width of line.
    , breakNewline   :: String     -- ^ Newline string.
    , breakContinue  :: String     -- ^ Newline string for breaking long line.
    , breakIndent    :: String     -- ^ Indent string for breaking long line.
    } deriving (Show, Eq, Ord)

-- | Same as 'crlfBreak'.
instance Def.Default LineBreak where
    def = crlfBreak

-- | LF: @"\\n"@
lfString :: String
lfString = "\n"

-- | CRLF: @"\\r\\n"@
crlfString :: String
crlfString = "\r\n"


-- ----------------------  Unlimited line width

unlimited :: String -> LineBreak
unlimited nl =
    LineBreak { breakWidth     = Nothing
              , breakNewline   = nl
              , breakContinue  = nl
              , breakIndent    = "" }

-- | Line break by single space.
noBreak :: LineBreak
noBreak = unlimited " "

-- | Line break by LF.
lfBreak :: LineBreak
lfBreak = unlimited lfString

-- | Line break by CRLF.
crlfBreak :: LineBreak
crlfBreak = unlimited crlfString


-- ----------------------  Limited line width

limited :: String -> Int -> Int -> LineBreak
limited nl ind wd =
    LineBreak { breakWidth     = Just wd
              , breakNewline   = nl
              , breakContinue  = nl
              , breakIndent    = replicate ind ' ' }

-- | Line break by LF with 2 spaces indent and given-length column.
lf2 :: Int -> LineBreak
-- | Line break by LF with 4 spaces indent and given-length column.
lf4 :: Int -> LineBreak
-- | Line break by LF with 8 spaces indent and given-length column.
lf8 :: Int -> LineBreak

-- | Line break by CRLF with 2 spaces indent and given-length column.
crlf2 :: Int -> LineBreak
-- | Line break by CRLF with 4 spaces indent and given-length column.
crlf4 :: Int -> LineBreak
-- | Line break by CRLF with 8 spaces indent and given-length column.
crlf8 :: Int -> LineBreak

lf2    = limited lfString 2
lf4    = limited lfString 4
lf8    = limited lfString 8
crlf2  = limited crlfString 2
crlf4  = limited crlfString 4
crlf8  = limited crlfString 8

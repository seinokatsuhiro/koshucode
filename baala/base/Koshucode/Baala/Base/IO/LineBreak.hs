{-# OPTIONS_GHC -Wall #-}

-- | Line break setting for mix text.

module Koshucode.Baala.Base.IO.LineBreak
  ( -- * Line break
    LineBreak (..),
    lfString, crlfString,

    -- * Unlimited line width
    noBreak, lfBreak, crlfBreak,

    -- * Limited line width
    lf804, crlf804,
    lf802, crlf802,
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

-- | No line break.
noBreak :: LineBreak
noBreak = unlimited ""

-- | LF line break.
lfBreak :: LineBreak
lfBreak = unlimited lfString

-- | CRLF line break.
crlfBreak :: LineBreak
crlfBreak = unlimited crlfString


-- ----------------------  Limited line width

limited :: Int -> Int -> String -> LineBreak
limited wd ind nl =
    LineBreak { breakWidth     = Just wd
              , breakNewline   = nl
              , breakContinue  = nl
              , breakIndent    = replicate ind ' ' }

-- | Line break by LF with 80 column and 2 spaces indent.
lf802 :: LineBreak
lf802 = limited 80 2 lfString

-- | Line break by CRLF with 80 column and 2 spaces indent.
crlf802 :: LineBreak
crlf802 = limited 80 2 crlfString

-- | Line break by LF with 80 column and 4 spaces indent.
lf804 :: LineBreak
lf804 = limited 80 4 lfString

-- | Line break by CRLF with 80 column and 4 spaces indent.
crlf804 :: LineBreak
crlf804 = limited 80 4 crlfString


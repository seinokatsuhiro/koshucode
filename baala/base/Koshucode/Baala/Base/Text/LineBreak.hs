{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.LineBreak
  ( -- * Line break
    LineBreak (..),
    nolb,
    lf804, crlf804,
    lf802, crlf802,
  ) where

import qualified Data.Default as Def

data LineBreak = LineBreak
  { breakWidth     :: Maybe Int
  , breakContinue  :: String
  , breakNewline   :: String
  , breakIndent    :: String
  } deriving (Show, Eq, Ord)

instance Def.Default LineBreak where
    def = nolb

nolb :: LineBreak
nolb =
    LineBreak { breakWidth     = Nothing
              , breakContinue  = ""
              , breakNewline   = ""
              , breakIndent    = "" }

lb :: Int -> Int -> String -> LineBreak
lb wd ind nl =
    LineBreak { breakWidth     = Just wd
              , breakContinue  = nl
              , breakNewline   = nl
              , breakIndent    = replicate ind ' ' }

lf802 :: LineBreak
crlf802 :: LineBreak
lf804 :: LineBreak
crlf804 :: LineBreak

lf802    = lb 80 2 "\n"
crlf802  = lb 80 2 "\r\n"
lf804    = lb 80 4 "\n"
crlf804  = lb 80 4 "\r\n"


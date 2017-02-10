{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Character sequence.

module Koshucode.Baala.Syntax.Symbol.Chars
  ( -- * Chars
    Chars, CharsMap,
    tChars, csT, stringChars,
    charsType,
  ) where

import qualified Koshucode.Baala.Overture         as O


#ifdef STRING_INPUT
-- | Character sequence.
type Chars = String

-- | Convert textual value to character sequence.
tChars :: (O.Textual t) => t -> Chars
tChars = O.tString

-- | Convert character sequence to textual value.
csT :: (O.Textual t) => Chars -> t
csT = O.stringT

-- | Type of input. (@"string"@)
charsType :: Chars
charsType = "string"

#else
-- | Character sequence.
type Chars = O.Tx

-- | Convert textual value to character sequence.
tChars :: (O.Textual t) => t -> Chars
tChars = O.tTx

-- | Convert character sequence to textual value.
csT :: (O.Textual t) => Chars -> t
csT = O.txT

-- | Type of input. (@"text"@)
charsType :: Chars
charsType = "text"

#endif

-- | Mapping from chars to chars.
type CharsMap = O.Map Chars

-- | Convert string to character sequence.
stringChars :: String -> Chars
stringChars = O.stringT

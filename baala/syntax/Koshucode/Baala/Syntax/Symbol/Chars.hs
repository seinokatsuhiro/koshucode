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


#if defined STRING_INPUT
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

#elif defined STRICT_TEXT_INPUT
-- | Character sequence.
type Chars = O.Tx

-- | Convert textual value to character sequence.
tChars :: (O.Textual t) => t -> Chars
tChars = O.tTx

-- | Convert character sequence to textual value.
csT :: (O.Textual t) => Chars -> t
csT = O.txT

-- | Type of input. (@"strict-text"@)
charsType :: Chars
charsType = "strict-text"

#elif defined LAZY_TEXT_INPUT
-- | Character sequence.
type Chars = O.Tz

-- | Convert textual value to character sequence.
tChars :: (O.Textual t) => t -> Chars
tChars = O.tTz

-- | Convert character sequence to textual value.
csT :: (O.Textual t) => Chars -> t
csT = O.tzT

-- | Type of input. (@"lazy-text"@)
charsType :: Chars
charsType = "lazy-text"

#endif

-- | Mapping from chars to chars.
type CharsMap = O.Map Chars

-- | Convert string to character sequence.
stringChars :: String -> Chars
stringChars = O.stringT

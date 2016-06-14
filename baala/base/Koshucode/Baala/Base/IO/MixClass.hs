{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Class for constructing mix text.

module Koshucode.Baala.Base.IO.MixClass
  ( -- * Class
    Mix (..),
    MixEncode (..),
    MixShortEncode (..),
    mixIdEncode,

    -- * Mix utility
    mixBracket, mixBracketS,
    mixJoin, mixJoinS,
    mixJoin1, mixJoinBar,
  ) where

import Data.Monoid ((<>))
import qualified Data.ByteString                      as Bs
import qualified Data.ByteString.Lazy                 as Bz
import qualified Data.Text                            as Tx
import qualified Data.Text.Lazy                       as Tz
import qualified Koshucode.Baala.Base.IO.MixText      as B
import qualified Koshucode.Baala.Base.Text            as B


-- ----------------------  Construct

-- | Construct mix text.
class Mix a where
    mix :: a -> B.MixText

-- | Mix text itself.
instance Mix B.MixText where
    mix = id

-- | Create mix text from strict bytestring.
instance Mix Bs.ByteString where
    mix = B.mixBs

-- | Create mix text from lazy bytestring.
instance Mix Bz.ByteString where
    mix = B.mixBz

-- | Create mix text from strict text.
instance Mix Tx.Text where
    mix = B.mixTx

-- | Create mix text from lazy text.
instance Mix Tz.Text where
    mix = B.mixTz

-- | Create mix text from string.
instance Mix String where
    mix = B.mixString

-- | Create mix text from char.
instance Mix Char where
    mix = B.mixChar

-- | Create mix text of given-length spaces.
instance Mix Int where
    mix = B.mixSpace

-- | Concatenate mix text.
instance Mix [B.MixText] where
    mix = mconcat

-- | Create empty mix text.
instance Mix () where
    mix _ = B.mixEmpty


-- ----------------------  Encode

-- | Encode via mix text.
class MixEncode a where
    mixEncode :: a -> B.MixText

-- | @(+)@ or @(-)@.
instance MixEncode Bool where
    mixEncode True  = B.mixString "(+)"
    mixEncode False = B.mixString "(-)"

-- | Encode with shortener.
class MixShortEncode a where
    mixShortEncode :: B.Shortener -> a -> B.MixText

-- | 'mixShortEncode' with no shortener.
mixIdEncode :: (MixShortEncode a) => a -> B.MixText
mixIdEncode = mixShortEncode B.nullShortener


-- ----------------------  Utility

-- | Enclose mix text with open and close bracket.
mixBracket :: (Mix m) => m -> m -> B.MixText -> B.MixText
mixBracket open close body = mix open <> body <> mix close

-- | Enclose mix text with bracket and space.
mixBracketS :: (Mix m) => m -> m -> B.MixText -> B.MixText
mixBracketS open close = mixBracket (mix open <> B.mix1) (B.mix1 <> mix close)

-- | Concatenate mix texts with delimiter.
mixJoin :: (Mix m) => m -> [B.MixText] -> B.MixText
mixJoin delim = loop where
    loop (x:xs) = x <> mix delim <> loop xs
    loop []     = B.mixEmpty

-- | Concatenate mix texts with delimiter and space.
mixJoinS :: (Mix m) => m -> [B.MixText] -> B.MixText
mixJoinS delim = mixJoin (B.mix1 <> mix delim <> B.mix1)

-- | Join with one space.
mixJoin1 :: [B.MixText] -> B.MixText
mixJoin1 = mixJoin B.mix1

-- | Join with vertical bar.
mixJoinBar :: [B.MixText] -> B.MixText
mixJoinBar = mixJoinS "|"


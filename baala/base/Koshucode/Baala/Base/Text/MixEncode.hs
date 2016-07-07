{-# OPTIONS_GHC -Wall #-}

-- | Encode using mix text.

module Koshucode.Baala.Base.Text.MixEncode
  ( -- * Encode
    MixEncode (..),
    MixShortEncode (..),
    mixIdEncode,
    Shorten, noShorten,
  ) where

import qualified Koshucode.Baala.Base.Text.MixText    as B

-- | Encode via mix text.
class MixEncode a where
    mixEncode :: a -> B.MixText

-- | @(+)@ or @(-)@.
instance MixEncode Bool where
    mixEncode True  = B.mixString "(+)"
    mixEncode False = B.mixString "(-)"

-- | Encode with shortener.
class MixShortEncode a where
    mixShortEncode :: Shorten -> a -> B.MixText

-- | 'mixShortEncode' with no shortener.
mixIdEncode :: (MixShortEncode a) => a -> B.MixText
mixIdEncode = mixShortEncode noShorten

-- | Convert string to short sign.
type Shorten = String -> Maybe String

-- | Shorten which does not shorten strings.
noShorten :: Shorten
noShorten _ = Nothing


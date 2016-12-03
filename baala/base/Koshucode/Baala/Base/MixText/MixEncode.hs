{-# OPTIONS_GHC -Wall #-}

-- | Encode using mix text.

module Koshucode.Baala.Base.MixText.MixEncode
  ( -- * Encode
    MixEncode (..),
    MixShortEncode (..),
    mixIdEncode,
    Shorten, noShorten,
  ) where

import qualified Koshucode.Baala.Base.MixText.MixText    as B

-- | Encode via mix text.
class MixEncode a where
    mixEncode :: a -> B.MixText

-- | @(+)@ or @(-)@.
instance MixEncode Bool where
    mixEncode True  = B.mixString "(+)"
    mixEncode False = B.mixString "(-)"

-- | Encode with shortener.
class MixShortEncode a where
    -- | Encode with string converter.
    mixShortEncode :: Shorten -> a -> B.MixText

    -- | 'mixShortEncode' with no shortener.
    mixPlainEncode :: a -> B.MixText
    mixPlainEncode = mixShortEncode noShorten

-- | Convert string to short sign.
type Shorten = String -> Maybe String

-- | Same as 'mixPlainEncode'.
{-# DEPRECATED mixIdEncode "Use 'mixPlainEncode' instead." #-}
mixIdEncode :: (MixShortEncode a) => a -> B.MixText
mixIdEncode = mixPlainEncode

-- | Shorten which does not shorten strings.
noShorten :: Shorten
noShorten _ = Nothing


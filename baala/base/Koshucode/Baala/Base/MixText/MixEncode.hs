{-# OPTIONS_GHC -Wall #-}

-- | Encode using mix text.

module Koshucode.Baala.Base.MixText.MixEncode
  ( -- * Encode
    MixEncode (..),
    MixShortEncode (..),
    TransString,
    mixShortEncode,
    mixIdEncode,
    noShorten,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base.MixText.MixText    as B

-- | Encode via mix text.
class MixEncode a where
    mixEncode :: a -> B.MixText

-- | @(+)@ or @(-)@.
instance MixEncode Bool where
    mixEncode True  = B.mixString "(+)"
    mixEncode False = B.mixString "(-)"

-- | Encode with transformer.
class MixShortEncode a where
    -- | Encode with string converter.
    mixTransEncode :: TransString -> a -> B.MixText

    -- | 'mixTransEncode' with no transformer.
    mixPlainEncode :: a -> B.MixText
    mixPlainEncode = mixTransEncode O.nothing

-- | Transform string to another form.
type TransString = String -> Maybe String

-- | Same as 'mixTransEncode'.
{-# DEPRECATED mixShortEncode "Use 'mixTransEncode' instead." #-}
mixShortEncode :: (MixShortEncode a) => TransString -> a -> B.MixText
mixShortEncode = mixTransEncode

-- | Same as 'mixPlainEncode'.
{-# DEPRECATED mixIdEncode "Use 'mixPlainEncode' instead." #-}
mixIdEncode :: (MixShortEncode a) => a -> B.MixText
mixIdEncode = mixPlainEncode

-- | Shorten which does not shorten strings.
{-# DEPRECATED noShorten "Use 'nothing' instead." #-}
noShorten :: TransString
noShorten _ = Nothing


{-# OPTIONS_GHC -Wall #-}

-- | Encode using mix text.

module Koshucode.Baala.Base.MixText.MixEncode
  ( -- * Encode
    MixEncode (..),
    encode,
    MixTransEncode (..),
    TransString,
    plainEncode,
    mixShortEncode,
    mixIdEncode,
    noShorten,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base.MixText.MixText    as B

-- ---------------------------------  MixEncode

-- | Encode via mix text.
class MixEncode a where
    mixEncode :: a -> B.MixText

-- | @(+)@ or @(-)@.
instance MixEncode Bool where
    mixEncode True  = B.mixString "(+)"
    mixEncode False = B.mixString "(-)"

-- | Encode to string.
encode :: (MixEncode a) => a -> String
encode = (B.mixToFlatString . mixEncode)

-- ---------------------------------  MixTransEncode

-- | Encode with transformer.
class MixTransEncode a where
    -- | Encode with string converter.
    mixTransEncode :: TransString -> a -> B.MixText

    -- | 'mixTransEncode' with no transformer.
    mixPlainEncode :: a -> B.MixText
    mixPlainEncode = mixTransEncode O.nothing

-- | Transform string to another form.
type TransString = String -> Maybe String

-- | Encode to string.
plainEncode :: (MixTransEncode a) => a -> String
plainEncode = (B.mixToFlatString . mixPlainEncode)

-- | Same as 'mixTransEncode'.
{-# DEPRECATED mixShortEncode "Use 'mixTransEncode' instead." #-}
mixShortEncode :: (MixTransEncode a) => TransString -> a -> B.MixText
mixShortEncode = mixTransEncode

-- | Same as 'mixPlainEncode'.
{-# DEPRECATED mixIdEncode "Use 'mixPlainEncode' instead." #-}
mixIdEncode :: (MixTransEncode a) => a -> B.MixText
mixIdEncode = mixPlainEncode

-- | Shorten which does not shorten strings.
{-# DEPRECATED noShorten "Use 'nothing' instead." #-}
noShorten :: TransString
noShorten _ = Nothing


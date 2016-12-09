{-# OPTIONS_GHC -Wall #-}

-- | Encode using mix text.

module Koshucode.Baala.Base.MixText.MixEncode
  ( MixEncode (..),
    TransString,
    encode,
    plainEncode,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base.MixText.MixText    as B

-- | Encode via mix text.
class MixEncode a where
    mixEncode :: a -> B.MixText
    mixEncode = mixTransEncode O.nothing

    mixTransEncode :: TransString -> a -> B.MixText
    mixTransEncode _ = mixEncode

-- | @(+)@ or @(-)@.
instance MixEncode Bool where
    mixEncode True  = B.mixString "(+)"
    mixEncode False = B.mixString "(-)"

-- | Transform string to another form.
type TransString = String -> Maybe String

-- | Encode to string.
encode :: (MixEncode a) => a -> String
encode = (B.mixToFlatString . mixEncode)

-- | Encode to string.
{-# DEPRECATED plainEncode "Use 'encode' instead." #-}
plainEncode :: (MixEncode a) => a -> String
plainEncode = encode


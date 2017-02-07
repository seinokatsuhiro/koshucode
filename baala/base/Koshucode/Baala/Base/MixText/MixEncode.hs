{-# OPTIONS_GHC -Wall #-}

-- | Encode using mix text.

module Koshucode.Baala.Base.MixText.MixEncode
  ( MixEncode (..),
    TransText,
    encode,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base.MixText.MixText    as B

-- | Encode via mix text.
class MixEncode a where
    mixEncode :: a -> B.MixText
    mixEncode = mixTransEncode (O.nothing :: TransText String)

    mixTransEncode :: (O.Textual t) => TransText t -> a -> B.MixText
    mixTransEncode _ = mixEncode

-- | @(+)@ or @(-)@.
instance MixEncode Bool where
    mixEncode True  = B.mixString "(+)"
    mixEncode False = B.mixString "(-)"

-- | Transform string to another form.
type TransText t = t -> Maybe t

-- | Encode to string.
encode :: (MixEncode a) => a -> String
encode = (B.mixToFlatString . mixEncode)


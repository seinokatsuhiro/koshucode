{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Class for constructing mix text.

module Koshucode.Baala.Base.IO.MixClass
  ( Mix (..),
    MixEncode (..),
  ) where

import qualified Data.ByteString                      as Bs
import qualified Data.ByteString.Lazy                 as Bz
import qualified Data.Text                            as Tx
import qualified Data.Text.Lazy                       as Tz
import qualified Koshucode.Baala.Base.IO.MixText      as B


-- ----------------------  Construct

-- | Construct mix text.
class Mix a where
    mix :: a -> B.MixText

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


{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Syntax.Symbol.Message
  ( -- * Message
    expOrdSym,
    quotNotEnd,
  ) where

import qualified Koshucode.Baala.Base       as B

-- | Expect ordinary symbol
expOrdSym :: B.Ab a
expOrdSym = B.leftBecause "Expect ordinary symbol"

-- | Quotation not end in line
quotNotEnd :: B.Ab a
quotNotEnd = B.leftBecause "Quotation not end in line"


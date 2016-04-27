{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Symbol.Message
  ( -- * Message
    expOrdSym,
    quotNotEnd,
  ) where

import qualified Koshucode.Baala.Base       as B

-- | Expect ordinary symbol
expOrdSym :: B.Ab a
expOrdSym = Left $ B.abortBecause "Expect ordinary symbol"

-- | Quotation not end in line
quotNotEnd :: B.Ab a
quotNotEnd = Left $ B.abortBecause "Quotation not end in line"


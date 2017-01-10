{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Syntax.Symbol.Message
  ( -- * Message
    expPlainSym,
    quotNotEnd,
  ) where

import qualified Koshucode.Baala.Base       as B

-- | [Expect plain symbol]
expPlainSym :: B.Ab a
expPlainSym = B.leftBecause "Expect plain symbol"

-- | [Quotation not end in line]
quotNotEnd :: B.Ab a
quotNotEnd = B.leftBecause "Quotation not end in line"


{-# OPTIONS_GHC -Wall #-}

-- | OperandGet

module Koshucode.Baala.Minimal.Relmap.Get
( OperandGet,
  getHead,
  getWord,
  getTerms,
  getTermPairs,
  getRelmap1
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Base.Syntax

type OperandGet v a
    = OpUse v      -- ^ Operater use
    -> String      -- ^ Lookup key
    -> AbortOr a   -- ^ Suboperand

{-| Abortable 'head' -}
getHead :: [a] -> AbortOr a
getHead (x:_) = Right x
getHead _     = Left $ AbortLookup [] "head"

{-| Get word from named operand. -}
getWord :: OperandGet v String
getWord use n = do
  let opd = halfOperand $ opHalf use
  sign <- opd <!!> n
  case sign of
    [TreeL (Word _ s)] -> Right s
    _ -> Left $ AbortLookup [] n

{-| Get list of term names from named operand. -}
getTerms :: OperandGet v [String]
getTerms use n = do
  let opd = halfOperand $ opHalf use
  term <- opd <!!> n
  termNames term

{-| Get list of term-name pairs from named operand. -}
getTermPairs :: OperandGet v [(String, String)]
getTermPairs use n = do
  let opd = halfOperand $ opHalf use
  term <- opd <!!> n
  termNamePairs term

getRelmap1 :: OpUse v -> AbortOr (Relmap v)
getRelmap1 use = do
  getHead $ opSub use


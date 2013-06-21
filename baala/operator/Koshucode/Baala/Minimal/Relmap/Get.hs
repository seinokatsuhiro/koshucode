{-# OPTIONS_GHC -Wall #-}

-- | Extract suboperand

module Koshucode.Baala.Minimal.Relmap.Get
( OpGet,
  getHead,
  getWord,
  getTerm1,
  getTerms,
  getTermPairs,
  getRelmap1
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Base.Syntax

type OpGet v a
    = OpUse v      -- ^ Operater use
    -> String      -- ^ Lookup key
    -> AbortOr a   -- ^ Suboperand

{-| Abortable 'head' -}
getHead :: [a] -> AbortOr a
getHead (x:_) = Right x
getHead _     = Left $ AbortLookup [] "head"

{-| Get word from named operand. -}
getWord :: OpGet v String
getWord use n = do
  let opd = halfOperand $ opHalf use
  sign <- opd <!!> n
  case sign of
    [TreeL (Word _ s)] -> Right s
    _ -> Left $ AbortLookup [] n

{-| Get list of term names from named operand. -}
getTerms :: OpGet v [String]
getTerms use n = do
  let opd = halfOperand $ opHalf use
  term <- opd <!!> n
  termNames term

{-| Get list of term names from named operand. -}
getTerm1 :: OpGet v String
getTerm1 use n = do
  ts <- getTerms use n
  case ts of
    [t] -> Right t
    _   -> Left $ AbortLookup [] n

{-| Get list of term-name pairs from named operand. -}
getTermPairs :: OpGet v [(String, String)]
getTermPairs use n = do
  let opd = halfOperand $ opHalf use
  term <- opd <!!> n
  termNamePairs term

getRelmap1 :: OpUse v -> AbortOr (Relmap v)
getRelmap1 use = do
  getHead $ opSub use


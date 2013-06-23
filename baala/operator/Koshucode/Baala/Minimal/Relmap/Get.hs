{-# OPTIONS_GHC -Wall #-}

-- | Extract suboperand

module Koshucode.Baala.Minimal.Relmap.Get
( -- * Generals
  getHead,

  -- * Get from OpUse
  OpGet,
  getWord,

  -- * Term from OpUse
  getTerm,
  getTerms,
  getTermPairs,

  -- * Relmap from OpUse
  getRelmap,
  getRelmaps
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Base.Syntax

{-| Abortable 'head' -}
getHead :: [a] -> AbortOr a
getHead (x:_) = Right x
getHead _     = Left $ AbortLookup [] "head"

type OpGet v a
    = OpUse v      -- ^ Operator use
    -> String      -- ^ Lookup key
    -> AbortOr a   -- ^ Suboperand

{-| Get word from named operand.

    > consXxx :: OpCons v
    > consXxx use = do
    >   sign <- getWord use "-sign"
    >   ...
    -}
getWord :: OpGet v String
getWord use n = do
  let opd = halfOperand $ opHalf use
  sign <- opd <!!> n
  case sign of
    [TreeL (Word _ s)] -> Right s
    _ -> Left $ AbortLookup [] n

{-| Get a term name from named operand. -}
getTerm :: OpGet v String
getTerm use n = do
  ts <- getTerms use n
  case ts of
    [t] -> Right t
    _   -> Left $ AbortLookup [] n

{-| Get list of term names from named operand. -}
getTerms :: OpGet v [String]
getTerms use n = do
  let opd = halfOperand $ opHalf use
  term <- opd <!!> n
  termNames term

{-| Get list of term-name pairs from named operand. -}
getTermPairs :: OpGet v [(String, String)]
getTermPairs use n = do
  let opd = halfOperand $ opHalf use
  term <- opd <!!> n
  termNamePairs term

{-| Get a relmap from operator use.

    > consMeet :: (Ord v) => OpCons v
    > consMeet use = do
    >   m <- getRelmap use
    >   Right $ relmapMeet use m
    -}

getRelmap :: OpUse v -> AbortOr (Relmap v)
getRelmap use = getHead $ opSub use

{-| Get relmaps from operator use. -}
getRelmaps :: OpUse v -> AbortOr [Relmap v]
getRelmaps use = Right $ opSub use


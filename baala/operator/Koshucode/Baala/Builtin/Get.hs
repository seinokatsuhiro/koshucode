{-# OPTIONS_GHC -Wall #-}

-- | Extract suboperand

module Koshucode.Baala.Builtin.Get
( -- * Generals
  getHead,

  -- * Get from OpUse
  OpGet,
  getTree,
  getTrees,
  getWord,
  getInt,

  -- * Term from OpUse
  getTerm,
  getTerms,
  getTermPair,
  getTermPairs,
  getTermTrees,

  -- * Relmap from OpUse
  getRelmap,
  getRelmaps,
) where

import Koshucode.Baala.Base
import Koshucode.Baala.Core hiding (getInt)
import Koshucode.Baala.Builtin.Term

{-| Abortable 'head' -}
getHead :: [a] -> AbortOr a
getHead (x:_) = Right x
getHead _     = Left (AbortLookup "head", [])

type OpGet v a
    = OpUse v      -- ^ Operator use
    -> String      -- ^ Lookup key
    -> AbortOr a   -- ^ Suboperand

getTree :: OpGet v TokenTree
getTree use n = do
  let opd = halfOperand $ opHalf use
  xs <- opd <!!> n
  Right $ TreeB 1 xs

getTrees :: OpGet v [TokenTree]
getTrees use n = do
  let opd = halfOperand $ opHalf use
  xs <- opd <!!> n
  Right xs

getTermTrees :: OpGet v [Named TokenTree]
getTermTrees use n = do
  let opd = halfOperand $ opHalf use
  xs <- opd <!!> n
  termTreePairs xs

{-| Get word from named operand.

    > consXxx :: RopCons v
    > consXxx use = do
    >   sign <- getWord use "-sign"
    >   ...
    -}
getWord :: OpGet v String
getWord use n = do
  let opd = halfOperand $ opHalf use
  sign <- opd <!!> n
  case sign of
    [TreeL (TWord _ _ s)] -> Right s
    _ -> Left (AbortLookup n, [])

getInt :: OpGet v Int
getInt use n = do
  let opd = halfOperand $ opHalf use
  sign <- opd <!!> n
  case sign of
    [TreeL (TWord _ _ i)] -> Right (read i :: Int)
    _ -> Left (AbortLookup n, [])

{-| Get a term name from named operand. -}
getTerm :: OpGet v String
getTerm use n = do
  ts <- getTerms use n
  case ts of
    [t] -> Right t
    _   -> Left (AbortLookup n, [])

{-| Get list of term names from named operand. -}
getTerms :: OpGet v [String]
getTerms use n = do
  let opd = halfOperand $ opHalf use
  term <- opd <!!> n
  termnames term

{-| Get list of term-name pairs from named operand. -}
getTermPairs :: OpGet v [Named String]
getTermPairs use n = do
  let opd = halfOperand $ opHalf use
  term <- opd <!!> n
  termnamePairs term

getTermPair :: OpGet v (Named String)
getTermPair use n = do
  let opd = halfOperand $ opHalf use
  term <- opd <!!> n
  termname2 term

{-| Get a relmap from operator use.

    > consMeet :: (Ord v) => RopCons v
    > consMeet use = do
    >   m <- getRelmap use
    >   Right $ relmapMeet use m
    -}
getRelmap :: OpUse v -> AbortOr (Relmap v)
getRelmap use = getHead $ opSubmap use

{-| Get relmaps from operator use. -}
getRelmaps :: OpUse v -> AbortOr [Relmap v]
getRelmaps use = Right $ opSubmap use


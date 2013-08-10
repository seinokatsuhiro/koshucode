{-# OPTIONS_GHC -Wall #-}

-- | Extract suboperand

module Koshucode.Baala.Builtin.Get
( -- * Generals
  getHead,

  -- * Get from RopUse
  OpGet,
  getTree,
  getTrees,
  getWord,
  getInt,

  -- * Term from RopUse
  getTerm,
  getTerms,
  getTermPair,
  getTermPairs,
  getTermTrees,

  -- * Relmap from RopUse
  getRelmap,
  getRelmaps,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Builtin.Term

{-| Abortable 'head' -}
getHead :: [a] -> B.AbortTokens a
getHead (x:_) = Right x
getHead _     = Left (B.AbortLookup "head", [])

type OpGet c a
    = C.RopUse c     -- ^ Operator use
    -> String        -- ^ Lookup key
    -> B.AbortTokens a   -- ^ Suboperand

getTree :: OpGet c B.TokenTree
getTree use n = do
  let opd = C.halfOperand $ C.ropHalf use
  xs <- opd B.<!!> n
  Right $ B.TreeB 1 xs

getTrees :: OpGet c [B.TokenTree]
getTrees use n = do
  let opd = C.halfOperand $ C.ropHalf use
  xs <- opd B.<!!> n
  Right xs

getTermTrees :: OpGet c [B.Named B.TokenTree]
getTermTrees use n = do
  let opd = C.halfOperand $ C.ropHalf use
  xs <- opd B.<!!> n
  termTreePairs xs

{-| Get word from named operand.

    > consXxx :: RopCons c
    > consXxx use = do
    >   sign <- getWord use "-sign"
    >   ...
    -}
getWord :: OpGet c String
getWord use n = do
  let opd = C.halfOperand $ C.ropHalf use
  trees <- opd B.<!!> n
  case trees of
    [B.TreeL (B.TWord _ _ s)] -> Right s
    _ -> Left (B.AbortLookup n, B.treesTokens trees)

getInt :: OpGet c Int
getInt use n = do
  let opd = C.halfOperand $ C.ropHalf use
  trees <- opd B.<!!> n
  case trees of
    [B.TreeL (B.TWord _ _ i)] -> Right (read i :: Int)
    _ -> Left (B.AbortLookup n, B.treesTokens trees)

{-| Get a term name from named operand. -}
getTerm :: OpGet c String
getTerm use n = do
  ts <- getTerms use n
  case ts of
    [t] -> Right t
    _   -> Left (B.AbortLookup n, [])

{-| Get list of term names from named operand. -}
getTerms :: OpGet c [String]
getTerms use n = do
  let opd  = C.halfOperand $ C.ropHalf use
  trees <- opd B.<!!> n
  termnames trees

{-| Get list of term-name pairs from named operand. -}
getTermPairs :: OpGet c [(String, String)]
getTermPairs use n = do
  let opd = C.halfOperand $ C.ropHalf use
  trees <- opd B.<!!> n
  termnamePairs trees

getTermPair :: OpGet c (String, String)
getTermPair use n = do
  let opd = C.halfOperand $ C.ropHalf use
  trees <- opd B.<!!> n
  termname2 trees

{-| Get a relmap from operator use.

    > consMeet :: (Ord c) => RopCons c
    > consMeet use = do
    >   m <- getRelmap use
    >   Right $ relmapMeet use m
    -}
getRelmap :: C.RopUse c -> B.AbortTokens (C.Relmap c)
getRelmap use = getHead $ C.ropSubmap use

{-| Get relmaps from operator use. -}
getRelmaps :: C.RopUse c -> B.AbortTokens [C.Relmap c]
getRelmaps use = Right $ C.ropSubmap use


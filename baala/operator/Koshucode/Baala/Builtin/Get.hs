{-# OPTIONS_GHC -Wall #-}

-- | Extract suboperand

module Koshucode.Baala.Builtin.Get
( -- * Generals
  getHead,
  getSingleton,

  -- * Get from RopUse
  RopGet,
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



-- ---------------------- Generals

{-| Abortable 'head' -}
getHead               :: [a] -> B.AbortTokens a
getHead (x : _)       = Right x
getHead _             = Left (B.AbortLookup "head", [])

getSingleton          :: [a] -> B.AbortTokens a
getSingleton [x]      = Right x
getSingleton _        = Left (B.AbortLookup "singleton", [])



-- ----------------------

type RopGet c b
    = C.RopUse c        -- ^ Use of relational operator
    -> String           -- ^ Lookup key
    -> B.AbortTokens b  -- ^ Suboperand

getTree :: RopGet c B.TokenTree
getTree use n =
    do trees <- getTrees use n
       Right $ B.TreeB 1 trees

getTrees :: RopGet c [B.TokenTree]
getTrees use n =
    do let operand = C.halfOperand $ C.ropHalf use
       operand B.<!!> n

getTermTrees :: RopGet c [B.Named B.TokenTree]
getTermTrees use n = getTrees use n >>= termTreePairs

{-| Get word from named operand.

    > consXxx :: RopCons c
    > consXxx use = do
    >   sign <- getWord use "-sign"
    >   ...
    -}
getWord :: RopGet c String
getWord use n =
    do trees <- getTrees use n
       case trees of
         [B.TreeL (B.TWord _ _ s)] -> Right s
         _ -> Left (B.AbortLookup n, B.treesTokens trees)

getInt :: RopGet c Int
getInt use n =
    do trees <- getTrees use n
       case trees of
         [B.TreeL (B.TWord _ _ i)] -> Right (read i :: Int)
         _ -> Left (B.AbortLookup n, B.treesTokens trees)



-- ----------------------  Terms

{-| Get a term name from named operand. -}
getTerm       :: RopGet c B.Termname
getTerm       use n = getTerms use n >>= getHead

{-| Get list of term names from named operand. -}
getTerms      :: RopGet c [B.Termname]
getTerms      use n = getTrees use n >>= termnames

getTermPair   :: RopGet c (B.Termname, B.Termname)
getTermPair   use n = getTermPairs use n >>= getSingleton

{-| Get list of term-name pairs from named operand. -}
getTermPairs  :: RopGet c [(B.Termname, B.Termname)]
getTermPairs  use n = getTrees use n >>= termnamePairs



-- ----------------------  Relmap

{-| Get a relmap from operator use.

    > consMeet :: (Ord c) => RopCons c
    > consMeet use = do
    >   m <- getRelmap use
    >   Right $ relmapMeet use m
    -}
getRelmap :: C.RopUse c -> B.AbortTokens (C.Relmap c)
getRelmap  use = getHead $ C.ropSubmap use

{-| Get relmaps from operator use. -}
getRelmaps :: C.RopUse c -> B.AbortTokens [C.Relmap c]
getRelmaps use = Right $ C.ropSubmap use


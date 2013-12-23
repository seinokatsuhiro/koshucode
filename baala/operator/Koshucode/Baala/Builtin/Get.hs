{-# OPTIONS_GHC -Wall #-}

{-| Extract suboperand -}

module Koshucode.Baala.Builtin.Get
( -- * Generals
  getHead,
  getSingleton,

  -- * Get from RopUse
  RopGet,
  getTree,
  getTrees,
  getMaybe,
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
getHead               :: [a] -> B.Ab a
getHead (x : _)       = Right x
getHead _             = Left $ B.abortNotFound "head"

getSingleton          :: [a] -> B.Ab a
getSingleton [x]      = Right x
getSingleton _        = Left $ B.abortNotFound "singleton"



-- ----------------------

type RopGet c b
    = C.RopUse c    -- ^ Use of relational operator
    -> String       -- ^ Lookup key
    -> B.Ab b       -- ^ Suboperand

operand :: C.RopUse c -> C.RopAssoc
operand = C.halfOperand . C.ropHalf

getMaybe :: RopGet c b -> RopGet c (Maybe b)
getMaybe get use n =
    case lookup n $ operand use of
      Nothing -> Right Nothing
      Just _  -> Right . Just =<< get use n

getTrees :: RopGet c [B.TokenTree]
getTrees use = (operand use B.<!!>)

getTree :: RopGet c B.TokenTree
getTree use n = Right . B.TreeB 1 =<< getTrees use n

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
         _ -> Left $ B.abortNotFound n

getInt :: RopGet c Int
getInt use n =
    do trees <- getTrees use n
       case trees of
         [B.TreeL (B.TWord _ _ i)] -> Right (read i :: Int)
         _ -> Left $ B.abortNotFound n



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
getRelmap :: C.RopUse c -> B.Ab (C.Relmap c)
getRelmap  use = getHead $ C.ropSubmap use

{-| Get relmaps from operator use. -}
getRelmaps :: C.RopUse c -> B.Ab [C.Relmap c]
getRelmaps use = Right $ C.ropSubmap use


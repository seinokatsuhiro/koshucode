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
  getOption,
  getSwitch,
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
import qualified Koshucode.Baala.Builtin.Term as Op



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

getOption :: b -> RopGet c b -> RopGet c b
getOption y get use n =
    do m <- getMaybe get use n
       case m of
         Just x  -> Right x
         Nothing -> Right y

getSwitch :: C.RopUse c -> String -> B.Ab Bool
getSwitch use n =
    do xs <- getMaybe getTrees use n
       case xs of
         Nothing -> Right False
         Just [] -> Right True
         _       -> Left $ B.abortNotFound n

getMap :: ([B.TokenTree] -> B.Ab b) -> RopGet c b
getMap f use n =
    do trees <- getTrees use n
       B.abortable "rop" (B.front $ B.untrees trees) $ f trees

getTrees :: RopGet c [B.TokenTree]
getTrees use = (operand use B.<!!>)

getTree :: RopGet c B.TokenTree
getTree use n = Right . B.TreeB 1 Nothing =<< getTrees use n

getTermTrees :: RopGet c [B.Named B.TokenTree]
getTermTrees = getMap Op.termTreePairs

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
getTerms      = getMap Op.termnames

getTermPair   :: RopGet c (B.Termname, B.Termname)
getTermPair   use n = getTermPairs use n >>= getSingleton

{-| Get list of term-name pairs from named operand. -}
getTermPairs  :: RopGet c [(B.Termname, B.Termname)]
getTermPairs = getMap Op.termnamePairs



-- ----------------------  Relmap

{-| Get a relmap from operator use.

    > consMeet :: (Ord c) => RopCons c
    > consMeet use = do
    >   m <- getRelmap use
    >   Right $ relmapMeet use m
    -}
getRelmap :: C.RopUse c -> B.Ab (C.Relmap c)
getRelmap  use = getHead $ C.ropSubrelmap use

{-| Get relmaps from operator use. -}
getRelmaps :: C.RopUse c -> B.Ab [C.Relmap c]
getRelmaps use = Right $ C.ropSubrelmap use


{-# OPTIONS_GHC -Wall #-}

{-| Extract suboperand from use of relmap -}

module Koshucode.Baala.Builtin.Get
( -- * Datatype
  RopGet,
  getMaybe,
  getOption,
  getTrees,

  -- * Relmap
  getRelmap,
  getRelmaps,

  -- * Term
  getTerm,
  getTerms,
  getTermPairs,
  getTermTrees,

  -- * Basic type
  getSwitch,
  getWord,
  getInt,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin.Term as Op



-- ---------------------- Utility

abortableTag :: String
abortableTag = "operand"

abortableGetTrees :: [B.TokenTree] -> B.Map (B.Ab b)
abortableGetTrees = B.abortable abortableTag . B.front . B.untrees

lookupOperand :: String -> C.RopUse c -> Maybe [B.TokenTree]
lookupOperand name = lookup name . C.halfOperand . C.ropHalf

getAbortable :: ([B.TokenTree] -> B.Ab b) -> RopGet c b
getAbortable f u name =
    do trees <- getTrees u name
       abortableGetTrees trees $ f trees

getAbortableOption :: b -> ([B.TokenTree] -> B.Ab b) -> RopGet c b
getAbortableOption y f u name =
    do m <- getMaybe getTrees u name
       case m of
         Nothing    -> Right y
         Just trees -> abortableGetTrees trees $ f trees


-- ----------------------  Datatype

type RopGet c b
    = C.RopUse c    -- ^ Use of relmap operator
    -> String       -- ^ Name of suboperand, e.g., @\"-term\"@
    -> B.Ab b       -- ^ Suboperand

getMaybe :: RopGet c b -> RopGet c (Maybe b)
getMaybe get u name =
    case lookupOperand name u of
      Nothing -> Right Nothing
      Just _  -> Right . Just =<< get u name

getOption :: b -> RopGet c b -> RopGet c b
getOption y get u name =
    case lookupOperand name u of
      Nothing -> Right y
      Just _  -> get u name

getTrees :: RopGet c [B.TokenTree]
getTrees u name =
    case lookupOperand name u of
      Just trees -> Right trees
      Nothing    -> Left $ B.AbortAnalysis [] $ B.AAOperandNotFound


-- ----------------------  Relmap

{-| Get a relmap from operator use.

    > consMeet :: (Ord c) => RopCons c
    > consMeet u = do
    >   m <- getRelmap u
    >   Right $ relmapMeet u m
    -}
getRelmap :: C.RopUse c -> B.Ab (C.Relmap c)
getRelmap u =
    do ms    <- getRelmaps u
       trees <- getTrees   u "-relmap"
       abortableGetTrees trees $ case ms of
         [m] -> Right m
         _   -> Left $ B.abortUnexpOperand "Require one relmap"

{-| Get relmaps from operator use. -}
getRelmaps :: C.RopUse c -> B.Ab [C.Relmap c]
getRelmaps = Right . C.ropSubrelmap


-- ----------------------  Term

{-| Get a term name from named operand. -}
getTerm :: RopGet c B.Termname
getTerm = getAbortable get where
    get [x] = Op.termname x
    get _   = Left $ B.abortUnexpOperand "Require one term"

{-| Get list of term names from named operand. -}
getTerms :: RopGet c [B.Termname]
getTerms = getAbortable Op.termnames

{-| Get list of term-name pairs from named operand. -}
getTermPairs :: RopGet c [(B.Termname, B.Termname)]
getTermPairs = getAbortable Op.termnamePairs

getTermTrees :: RopGet c [B.Named B.TokenTree]
getTermTrees = getAbortable Op.termTreePairs


-- ----------------------  Basic type

getSwitch :: C.RopUse c -> String -> B.Ab Bool
getSwitch u name = getAbortableOption False get u name where
    get [] = Right True
    get _  = Left $ B.abortUnexpOperand $ "Just type only " ++ name

{-| Get word from named operand.

    > consXxx :: RopCons c
    > consXxx u = do
    >   sign <- getWord u "-sign"
    >   ...
    -}
getWord :: RopGet c String
getWord = getAbortable get where
    get [B.TreeL (B.TWord _ _ s)] = Right s
    get _ = Left $ B.abortUnexpOperand "Require one word"

getInt :: RopGet c Int
getInt = getAbortable get where
    get [B.TreeL (B.TWord _ _ i)] = Right (read i :: Int)
    get _ = Left $ B.abortUnexpOperand "Require one integer"


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Builtin.Operator
( ropList,
  builtinRops,

  -- $ListOfOperator
) where

import qualified Data.List as List
import qualified Data.Monoid as M
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C

{-| Make implementations of relation-mapping operators. -}
ropList
    :: String      -- ^ Operator group
    -> [(String, C.RopCons c, C.RopOperand)]
                   -- ^ Synopsis, constructor, and operand sorter
    -> [C.Rop c]   -- ^ List of relation-mapping operators
ropList group = map rop where
    rop (synopsis, cons, opd) =
        let slist  = words synopsis
            name   = head slist
            sorter = operandSorter opd
        in C.Rop name group sorter cons synopsis

operandSorter :: C.RopOperand -> C.RopFullSorter
operandSorter (trunkSorter, trunkTerms, branchTerms) xs = xs3 where
    allTerms = trunkTerms ++ branchTerms
    xs2  = C.sortOperand xs
    ns2  = map fst xs2

    more = B.assocMore $ B.assocGather xs2
    unk  = ns2  List.\\  ("" : allTerms)
    wrap = ns2 `List.intersect` trunkTerms

    xs3  | not (null more) = Left $ B.AbortOpeandDuplicate (map fst more)
         | not (null unk)  = Left $ B.AbortOpeandUnknown unk
         | not (null wrap) = Right xs2
         | otherwise       = trunkSorter xs2

{-| Built-in relation-mapping operator. -}
builtinRops :: [C.Rop c]
builtinRops = ropList "builtin" [ ("|", ropConsConcat, C.operandList "--" []) ]

ropConsConcat :: C.RopCons c
ropConsConcat = Right . M.mconcat . C.ropSubmap

-- ----------------------
{- $ListOfOperator

   [/r/ @|@ /s/]   Append relmaps

-}


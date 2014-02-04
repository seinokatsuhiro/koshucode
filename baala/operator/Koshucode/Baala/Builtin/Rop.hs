{-# OPTIONS_GHC -Wall #-}

-- | Built-in relmap operator.

module Koshucode.Baala.Builtin.Rop
( builtinRops,
  ropList,

  -- $ListOfOperator
) where

import qualified Data.Monoid as M
import qualified Koshucode.Baala.Core as C


{-| Built-in relmap operator. -}
builtinRops :: [C.Rop c]
builtinRops = ropList "builtin"
    [ ("append R ...", ropConsConcat, C.sortList "-relmap" []) ]

ropConsConcat :: C.RopCons c
ropConsConcat = Right . M.mconcat . C.ropSubrelmap

{-| Make implementations of relation-mapping operators. -}
ropList
    :: String      -- ^ Operator group
    -> [(String, C.RopCons c, C.RopOperandSorter)]
                   -- ^ Synopsis, constructor, and operand sorter
    -> [C.Rop c]   -- ^ List of relation-mapping operators
ropList group = map rop where
    rop (synopsis, cons, opd) =
        let name   = head $ words synopsis
            sorter = C.ropFullSorter opd
        in C.Rop name group sorter cons synopsis



-- ----------------------
{- $ListOfOperator

   [/r/ @|@ /s/]   Append relmaps

-}


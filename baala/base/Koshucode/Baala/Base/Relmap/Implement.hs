{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Relmap.Implement
( RelmapImplement (..),
  OperandParser,
  OperandParser',
  OpCons
) where

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Relmap.Relmap
import Koshucode.Baala.Base.Syntax

{-| Implementation of relmap operator.
    It consists of
    (1) operator name,
    (2) operand parser,
    (3) constructor of operator, and
    (4) usage of operator. -}
data RelmapImplement v =
    RelmapImplement String OperandParser (OpCons v) [String]
    
{-| Parser for operand of relational operator.
    This parsers docompose operand trees,
    and give a name to suboperand. -}
type OperandParser = [TokenTree] -> [Named [TokenTree]]

type OperandParser' = [Named [TokenTree]] -> [Named [TokenTree]]

{-| Constructor of relational operator 'Relmap'.
    'Relmap' is constructed from 'HalfRelmap' and subrelmaps in it. -}
type OpCons v = OpUse v -> AbortOr (Relmap v)


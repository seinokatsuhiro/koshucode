{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Relmap.Implement
( RelmapImplement (..),
  OperandParser,
  OperatorCons
) where

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Relmap.HalfRelmap
import Koshucode.Baala.Base.Relmap.Relmap
import Koshucode.Baala.Base.Syntax

{-| Implementation of relmap operator.
    It consists of
    (1) operator name,
    (2) operand parser,
    (3) constructor of operator, and
    (4) usage of operator. -}
data RelmapImplement v =
    RelmapImplement String OperandParser (OperatorCons v) [String]
    
{-| Parser for operand of relational operator.
    This parsers docompose operand trees,
    and give a name to suboperand. -}
type OperandParser = [TokenTree] -> [Named [TokenTree]]

{-| Constructor of relational operator 'Relmap'.
    'Relmap' is constructed from 'HalfRelmap' and subrelmaps in it. -}
type OperatorCons v = [Relmap v] -> HalfRelmap -> AbortOr (Relmap v)


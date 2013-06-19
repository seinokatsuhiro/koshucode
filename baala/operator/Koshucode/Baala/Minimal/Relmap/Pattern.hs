{-# OPTIONS_GHC -Wall #-}

-- | Class for operand patterns

module Koshucode.Baala.Minimal.Relmap.Pattern
( OperandPattern (..)
, relmaps
) where

import Koshucode.Baala.Minimal.OpeKit as Kit

-- | Class for operand pattern.
class OperandPattern p where
    operandParser :: p -> OperandParser
    operandParser p = operandParser' p . operandGroup

    operandParser' :: p -> OperandParser'
    operandParser' _ = id

    operandUsage :: p -> [String]
    operandUsage _ = []

-- | Make relmap implementations.
relmaps
    :: (OperandPattern p)
    => [(String, p, OpCons v)] -- ^ Operator implementations
    -> [RelmapImplement v] -- ^ Result
relmaps = map f where
    f (op, p, full) =
        let parser  = operandParser p
            addOp u = op ++ " " ++ u
        in RelmapImplement op parser full (map addOp $ operandUsage p)


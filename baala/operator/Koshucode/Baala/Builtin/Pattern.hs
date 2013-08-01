{-# OPTIONS_GHC -Wall #-}

-- | Class for operand patterns

module Koshucode.Baala.Builtin.Pattern
( OpPattern (..),
  operators,
) where

import Koshucode.Baala.Core

{-| Class for operand pattern. -}
class OpPattern p where
    {-| Operand parser. -}
    opParser :: p -> OpParser
    opParser p = opParser' p . operandGroup

    {-| Simplified operand parser. -}
    opParser' :: p -> OpParser'
    opParser' _ = id

    {-| Names of suboperands. -}
    opPart :: p -> [String]

    {-| Synopsis. -}
    opUsage :: p -> [String]

{-| Make implementations of relational operators. -}
operators
    :: (OpPattern p)
    => String                   -- ^ Operator group
    -> [(String, p, RopCons v)] -- ^ Operator implementations
    -> [Rop v]                  -- ^ Implementation list
operators g = map f where
    f (op, pat, cons) =
        let parser  = opParser pat
            addOp u = op ++ " " ++ u
        in Rop op g parser cons (map addOp $ opUsage pat)


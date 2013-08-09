{-# OPTIONS_GHC -Wall #-}

-- | Class for operand patterns

module Koshucode.Baala.Builtin.Pattern
( OpPattern (..),
  operators,
) where

import qualified Koshucode.Baala.Core as C

{-| Class for operand pattern. -}
class OpPattern p where
    {-| Operand parser. -}
    opParser :: p -> C.RopParser
    opParser p = opParser' p . C.operandGroup

    {-| Simplified operand parser. -}
    opParser' :: p -> C.RopParser'
    opParser' _ = id

    {-| Names of suboperands. -}
    opPart :: p -> [String]

    {-| Synopsis. -}
    opUsage :: p -> [String]

{-| Make implementations of relational operators. -}
operators
    :: (OpPattern p)
    => String                     -- ^ Operator group
    -> [(String, p, C.RopCons v)] -- ^ Operator implementations
    -> [C.Rop v]                  -- ^ Implementation list
operators g = map f where
    f (op, pat, cons) =
        let parser  = opParser pat
            addOp u = op ++ " " ++ u
        in C.Rop op g parser cons (map addOp $ opUsage pat)


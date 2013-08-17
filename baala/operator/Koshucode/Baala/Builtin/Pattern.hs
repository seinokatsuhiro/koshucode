{-# OPTIONS_GHC -Wall #-}

{-| Class for operand patterns -}

module Koshucode.Baala.Builtin.Pattern
( RopPattern (..),
  ropGroup,
) where

import qualified Koshucode.Baala.Core as C

{-| Class for operand pattern. -}
class RopPattern p where

    {-| Operand sorter. -}
    ropSorter :: p -> C.RopSorter
    ropSorter _ = id

    {-| Names of suboperands. -}
    ropPart   :: p -> [String]

    {-| Synopsis. -}
    ropUsage  :: p -> [String]

{-| Make implementations of relational operators. -}
ropGroup
    :: (RopPattern p)
    => String                     -- ^ Operator group
    -> [(String, p, C.RopCons c)] -- ^ Operator name and constructor
    -> [C.Rop c]                  -- ^ Implementation list
ropGroup g = map f where
    f (op, pat, cons) =
        let sorter  = ropSorter pat . C.operandGroup
            addOp u = op ++ " " ++ u
        in C.Rop op g sorter cons (map addOp $ ropUsage pat)


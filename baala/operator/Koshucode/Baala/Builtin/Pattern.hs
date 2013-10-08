{-# OPTIONS_GHC -Wall #-}

{-| Class for operand patterns -}

module Koshucode.Baala.Builtin.Pattern
( RopPattern (..),
  ropList,
) where

import qualified Koshucode.Baala.Core as C

{-| Class for operand pattern. -}
class RopPattern p where

    {-| Operand sorter. -}
    ropSorter :: p -> C.RopSorter
    ropSorter _ x = Right x

    {-| Names of suboperands. -}
    ropPart   :: p -> [String]

{-| Make implementations of relational operators. -}
ropList
    :: (RopPattern p)
    => String                     -- ^ Operator group
    -> [(String, p, C.RopCons c)] -- ^ Operator name and constructor
    -> [C.Rop c]                  -- ^ Implementation list
ropList group = map f where
    f (synopsis, p, cons) =
        let slist  = words synopsis
            name   = head slist
            sorter = ropSorter p . C.sortOperand
        in C.Rop name group sorter cons synopsis


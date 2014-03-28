{-# OPTIONS_GHC -Wall #-}

-- | Relational operators for nested relations.

module Koshucode.Baala.Op.Nest.Rop
( nestRops,
  -- $Operators
) where

import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Op.Builtin         as Op
import qualified Koshucode.Baala.Op.Nest.Confl      as Op

-- | Implementation of relational operators.
nestRops :: (C.CContent c) => [C.Rop c]
nestRops = Op.ropList "nest"
    --  SYNOPSIS,
    --  CONSTRUCTOR, OPERAND
    [ ( "rel-add /N R [ -with /N ... ]",
        Op.consRelAdd, C.sortTwo "-term" "-relmap" ["-with"] )
    , ( "rel-down /N",
        Op.consRelDown, C.sortOne "-term" [] )
    , ( "group /N R",
        Op.consGroup, C.sortTwo "-term" "-relmap" [] )
    ]

-- ----------------------
-- $Operators
--
--  [@rel-add \/N R@]
--
--  [@rel-down \/N@]
--    Enclose input relation in a term.
--
--  [@group \/N R@]
--    Group tuples in @R@ by input relation.
--

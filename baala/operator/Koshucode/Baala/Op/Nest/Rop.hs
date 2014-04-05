{-# OPTIONS_GHC -Wall #-}

-- | Relational operators for nested relations.

module Koshucode.Baala.Op.Nest.Rop
( nestRops,
  -- $Operators
) where

import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Op.Builtin         as Op
import qualified Koshucode.Baala.Op.Nest.Confl      as Op
import qualified Koshucode.Baala.Op.Nest.Flow       as Op

-- | Implementation of relational operators.
nestRops :: (C.CContent c) => [C.Rop c]
nestRops = Op.ropList "nest"
    --  SYNOPSIS,
    --  CONSTRUCTOR, OPERAND
    [ ( "down /N",
        Op.consDown, C.sortOne "-term" [] )
    , ( "for /N R [ -with /N ... ]",
        Op.consFor, C.sortTwo "-term" "-relmap" ["-with"] )
    , ( "group /N R",
        Op.consGroup, C.sortTwo "-term" "-relmap" [] )
    , ( "group-by /N R",
        Op.consGroupBy, C.sortTwo "-term" "-relmap" [] )
    , ( "copy N R",
        Op.consCopy, C.sortTwo "-name" "-relmap" [] )
    , ( "slice /N R [ -with /N ... ]",
        Op.consSlice, C.sortTwo "-term" "-relmap" ["-with"] )
    , ( "up /N",
        Op.consUp, C.sortOne "-term" [] )
    ]


-- ----------------------
-- $Operators
--
--  [@down \/N@]
--    Enclose input relation in a term @\/N@.
--
--  [@for \/P R@]
--    Convert nested relation @\/P@ by relmap @R@.
--
--  [@copy N R@]
--    Naming input relation as @N@ in relmap @R@.
--
--  [@group \/N R@]
--    Group tuples in @R@ by input relation.
--
--  [@slice \/N R@]
--    Add nested relation as output of relmap @R@.
--
--  [@up \/P@]
--    Lift up nested relation.
--

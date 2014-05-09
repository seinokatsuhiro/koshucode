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
    [ ( "copy N R",
        Op.consCopy, C.rodTwo "-with" "-relmap" [] )
    , ( "down /N",
        Op.consDown, C.rodOne "-term" [] )
    , ( "for /N R [ -with /N ... ]",
        Op.consFor, C.rodTwo "-term" "-relmap" ["-with"] )
    , ( "group /N R",
        Op.consGroup, C.rodTwo "-term" "-relmap" [] )
    , ( "group-by /N R",
        Op.consGroupBy, C.rodTwo "-term" "-relmap" [] )
    , ( "join-up /P ...",
        Op.consJoinUp, C.rodList "-term" [] )
    , ( "nest /P ... -to /N",
        Op.consNest, C.rodList "-term" ["-to"] )
    , ( "unnest /P",
        Op.consUnnest, C.rodList "-term" [] )
    , ( "slice /N [R] [ -with /N ... ]",
        Op.consSlice, C.rodOneOpt "-term" "-relmap" ["-with"] )
    , ( "slice-up R [ -with /N ... ]",
        Op.consSliceUp, C.rodOne "-relmap" ["-with"] )
    , ( "split /N E ...",
        Op.consSplit, C.rodList "-in" ["-let"] )
    , ( "up /N",
        Op.consUp, C.rodOne "-term" [] )
    ]


-- ----------------------
-- $Operators
--
--  [@copy N R@]
--    Naming input relation as @N@ in relmap @R@.
--
--  [@down \/N@]
--    Enclose input relation in a term @\/N@.
--
--  [@for \/P R@]
--    Convert nested relation @\/P@ by relmap @R@.
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

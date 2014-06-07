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
    --  CONSTRUCTOR, ATTRIBUTE
    [ ( "chunk /T ... [-order /P ...]",
        Op.consChunk, C.roaList "-term" ["-order"] )
    , ( "copy N R",
        Op.consCopy, C.roaTwo "-with" "-relmap" [] )
    , ( "down /N",
        Op.consDown, C.roaOne "-term" [] )
    , ( "for /N R [ -with /N ... ]",
        Op.consFor, C.roaTwo "-term" "-relmap" ["-with"] )
    , ( "group /N R",
        Op.consGroup, C.roaTwo "-term" "-relmap" [] )
    , ( "group-by /N R",
        Op.consGroupBy, C.roaTwo "-term" "-relmap" [] )
    , ( "join-up /P ...",
        Op.consJoinUp, C.roaList "-term" [] )
    , ( "nest [~] /P ... -to /N",
        Op.consNest, C.roaList "-term" ["-to"] )
    , ( "unnest /P",
        Op.consUnnest, C.roaList "-term" [] )
    , ( "slice /N [R] [-with /N ...]",
        Op.consSlice, C.roaOneOpt "-term" "-relmap" ["-with"] )
    , ( "slice-up R [-with /N ...]",
        Op.consSliceUp, C.roaOne "-relmap" ["-with"] )
    , ( "up /N",
        Op.consUp, C.roaOne "-term" [] )
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

{-# OPTIONS_GHC -Wall #-}

-- | Relational operators for nested relations.

module Koshucode.Baala.Op.Nest.Rop
( ropsNest,
) where

import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Op.Builtin         as Op
import qualified Koshucode.Baala.Op.Nest.Confl      as Op
import qualified Koshucode.Baala.Op.Nest.Flow       as Op

-- | Implementation of relational operators.
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

ropsNest :: (C.CContent c) => [C.Rop c]
ropsNest = Op.ropList "nest"
    --          CONSTRUCTOR     USAGE                          ATTRIBUTE
    [ Op.ropV   Op.consChunk    "chunk /T ... [-order /P ...]" "-term | -order"
    , Op.ropII  Op.consCopy     "copy N R"                     "-with -relmap/"
    , Op.ropI   Op.consDown     "down /N"                      "-term"
    , Op.ropII  Op.consFor      "for /N R [-with /N ...]"      "-term -relmap/ | -with"
    , Op.ropII  Op.consGroup    "group /N R"                   "-term -relmap/"
    , Op.ropII  Op.consGroupBy  "group-by /N R"                "-term -relmap/"
    , Op.ropV   Op.consJoinUp   "join-up /P ..."               "-term"
    , Op.ropV   Op.consNest     "nest [~] /P ... -to /N"       "-term | -to"
    , Op.ropI   Op.consHang     "hang /N -on /P ..."           "-term | -on"
    , Op.ropV   Op.consUnnest   "unnest /P"                    "-term"
    , Op.ropIJ  Op.consSlice    "slice /N [R] [-with /N ...]"  "-term -relmap/ | -with"
    , Op.ropI   Op.consSliceUp  "slice-up R [-with /N ...]"    "-relmap/ | -with"
    , Op.ropI   Op.consUp       "up /N"                        "-term"
    ]


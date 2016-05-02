{-# OPTIONS_GHC -Wall #-}

-- | Relational operators for nested relations.

module Koshucode.Baala.Rop.Nest.Rop
  ( ropsNest,
  ) where

import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Op
import qualified Koshucode.Baala.Rop.Nest.Confl     as Op
import qualified Koshucode.Baala.Rop.Nest.Deriv     as Op
import qualified Koshucode.Baala.Rop.Nest.Flow      as Op

-- | Implementation of relational operators.
--
--  [@chunk \/N ... \[-order \/P ...\]@]
--    Split input relation into some chunks.
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
--  [@group-by \/N R@]
--    Group input relation by output of @R@.
--
--  [@hang \/N -on \/P ...@]
--    Hang nested relation @\/N@ on @\/P@ ....
--
--  [@join-up \/P ...@]
--    Join @\/P@ ... and up the result relation.
--
--  [@nest \/P ... -to \/N@]
--    Nest terms @\/P@ ... into nested relation.
--
--  [@unnest \/P@]
--    Unnest nested relation @\/P@.
--
--  [@slice \/N \[R\]@]
--    Add nested relation as output of relmap @R@.
--
--  [@slice-up R@]
--    Slice, nested map, and lift up.
--
--  [@up \/P@]
--    Lift up nested relation.

ropsNest :: (D.CContent c) => [C.Rop c]
ropsNest = Op.ropList "nest"
    --        CONSTRUCTOR       USAGE                           ATTRIBUTE
    [ Op.def  Op.consChunk      "chunk /T ... [-order /P ...]"  "V -term | -order"
    , Op.def  Op.consCopy       "copy N R"                      "2 -var -relmap/^"
    , Op.def  Op.consDown       "down /N"                       "1 -term"
    , Op.def  Op.consFor        "for /N R"                      "2 -term -relmap/^"
    , Op.def  Op.consGroup      "group R -to /N "               "1  -relmap/ | -to"
    , Op.def  Op.consJoinUp     "join-up /P ..."                "V -term"
    , Op.def  Op.consNest       "nest [~] /P ... -to /N"        "V -term | -to"
    , Op.def  Op.consOppGroup   "opp-group R -to /N"            "1 -relmap/ | -to"
    , Op.def  Op.consPickGroup  "pick-group /P ... -to /N"      "V -term | -to"
    , Op.def  Op.consPickGroup  "hier /P ... -to /N"            "V -term | -to"
    , Op.def  Op.consSlice      "slice /N [R]"                  "1? -term -relmap/^"
    , Op.def  Op.consSliceUp    "slice-up R"                    "1 -relmap/^"
    , Op.def  Op.consUngroup    "ungroup /P"                    "1 -term"
    , Op.def  Op.consUp         "up /N"                         "1 -term"
    ]

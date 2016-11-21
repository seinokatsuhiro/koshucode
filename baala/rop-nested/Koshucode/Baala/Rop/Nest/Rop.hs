{-# OPTIONS_GHC -Wall #-}

-- | Relational operators for nested relations.

module Koshucode.Baala.Rop.Nest.Rop
  ( ropsNest,
  ) where

import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Rop
import qualified Koshucode.Baala.Rop.Nest.Confl     as Rop
import qualified Koshucode.Baala.Rop.Nest.Deriv     as Rop
import qualified Koshucode.Baala.Rop.Nest.Flow      as Rop

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
--  [@hang \/N -on \/P ...@]
--    Hang nested relation @\/N@ on @\/P@ ....
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
ropsNest = Rop.ropList "nest"
    --        CONSTRUCTOR        USAGE                       ATTRIBUTE
    [ Rop.def Rop.consChunk      "chunk /T ... [-order /P ...]"
                                                             "-term* . -order"
    , Rop.def Rop.consCopy       "copy N R"                  "-var -relmap/^"
    , Rop.def Rop.consDown       "down /N"                   "-term"
    , Rop.def Rop.consFor        "for /N R"                  "-term -relmap/^"
    , Rop.def Rop.consGroup      "group R -to /N [-share /P ...]"
                                                             "-relmap/ . -to -share?"
    , Rop.def Rop.consJoinUp     "join-up /P ..."            "-term*"
    , Rop.def Rop.consNest       "nest [~] /P ... -to /N"    "-term* . -to"
    , Rop.def Rop.consOppGroup   "opp-group R -to /N [-share /P ...]"
                                                             "-relmap/ . -to -share?"
    , Rop.def Rop.consSelfGroup  "self-group /P ... -to /N"  "-term* . -to"
    , Rop.def Rop.consSelfGroup  "sg /P ... -to /N"          "-term* . -to"
    , Rop.def Rop.consSelfGroup  "hier /P ... -to /N"        "-term* . -to"
    , Rop.def Rop.consSlice      "slice /N [R]"              "-term -relmap/^?"
    , Rop.def Rop.consSliceUp    "slice-up R"                "-relmap/^"
    , Rop.def Rop.consUngroup    "ungroup /P"                "-term"
    , Rop.def Rop.consUp         "up /N"                     "-term"
    ]


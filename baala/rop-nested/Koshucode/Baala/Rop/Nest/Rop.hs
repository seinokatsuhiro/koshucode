{-# OPTIONS_GHC -Wall #-}

-- | Relational operators for nested relations.

module Koshucode.Baala.Rop.Nest.Rop
  ( ropsNest,
  ) where

import Koshucode.Baala.Overture ((&))
import qualified Koshucode.Baala.Overture           as O
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
ropsNest = Rop.ropAlias
    [ "sg"   & "self-group"
    , "hier" & "self-group"
    ] $ Rop.ropList "nest"
    --        CONSTRUCTOR          USAGE                                    ATTRIBUTE
    [ Rop.rop Rop.consChunk      [ "chunk /T ... [-order /P ...]"       O.& "-term* . -order" ]
    , Rop.rop Rop.consCopy       [ "copy N R"                           O.& "-var -relmap/^" ]
    , Rop.rop Rop.consDown       [ "down /N"                            O.& "-term" ]
    , Rop.rop Rop.consFor        [ "for /N R"                           O.& "-term -relmap/^" ]
    , Rop.rop Rop.consGroup      [ "group R -to /N [-share /P ...]"     O.& "-relmap/ . -to -share?" ]
    , Rop.rop Rop.consJoinUp     [ "join-up /P ..."                     O.& "-term*" ]
    , Rop.rop Rop.consNest       [ "nest [~] /P ... -to /N"             O.& "-term* . -to" ]
    , Rop.rop Rop.consOppGroup   [ "opp-group R -to /N [-share /P ...]" O.& "-relmap/ . -to -share?" ]
    , Rop.rop Rop.consSelfGroup  [ "self-group /P ... -to /N"           O.& "-term* . -to" ]
    , Rop.rop Rop.consSlice      [ "slice /N [R]"                       O.& "-term -relmap/^?" ]
    , Rop.rop Rop.consSliceUp    [ "slice-up R"                         O.& "-relmap/^" ]
    , Rop.rop Rop.consUngroup    [ "ungroup /P"                         O.& "-term" ]
    , Rop.rop Rop.consUp         [ "up /N"                              O.& "-term" ]
    ]


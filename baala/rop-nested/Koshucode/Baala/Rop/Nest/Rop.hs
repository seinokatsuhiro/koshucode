{-# OPTIONS_GHC -Wall #-}

-- | Relational operators for nested relations.

module Koshucode.Baala.Rop.Nest.Rop
  ( ropsNest,
  ) where

import qualified Koshucode.Baala.DataPlus           as K
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

ropsNest :: (K.CContent c) => [C.Rop c]
ropsNest = Rop.ropAlias
    [ "sg"   K.& "self-group"
    , "hier" K.& "self-group"
    ] $ Rop.rops "nest"
    [ Rop.consChunk      K.& [ "chunk /T ... [-order /P ...]"       K.& "-term* . -order" ]
    , Rop.consCopy       K.& [ "copy N R"                           K.& "-var -relmap/^" ]
    , Rop.consDown       K.& [ "down /N"                            K.& "-term" ]
    , Rop.consFor        K.& [ "for /N R"                           K.& "-term -relmap/^" ]
    , Rop.consGroup      K.& [ "group R -to /N [-share /P ...]"     K.& "-relmap/ . -to -share?" ]
    , Rop.consJoinUp     K.& [ "join-up /P ..."                     K.& "-term*" ]
    , Rop.consNest       K.& [ "nest [~] /P ... -to /N"             K.& "-term* . -to" ]
    , Rop.consOppGroup   K.& [ "opp-group R -to /N [-share /P ...]" K.& "-relmap/ . -to -share?" ]
    , Rop.consSelfGroup  K.& [ "self-group /P ... -to /N"           K.& "-term* . -to" ]
    , Rop.consSlice      K.& [ "slice /N [R]"                       K.& "-term -relmap/^?" ]
    , Rop.consSliceUp    K.& [ "slice-up R"                         K.& "-relmap/^" ]
    , Rop.consUngroup    K.& [ "ungroup /P"                         K.& "-term" ]
    , Rop.consUp         K.& [ "up /N"                              K.& "-term" ]
    ]


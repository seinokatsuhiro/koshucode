{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators on relational lattice.

module Koshucode.Baala.Op.Lattice.Rop
( ropsLattice,
) where

import qualified Koshucode.Baala.Core                 as C
import qualified Koshucode.Baala.Op.Builtin           as Op
import qualified Koshucode.Baala.Op.Lattice.Restrict  as Op
import qualified Koshucode.Baala.Op.Lattice.Tropashko as Op


-- | Lattice implementations of relmap operators.
--
--   [@join@]    Join two relations.
--
--   [@meet@]    Meet two relations.
--
--   [@none@]    Restriction by relmaps.
--
--   [@some@]    Restriction by relmaps.
--
--   [@sub@]     Restriction to subrelation.
--
ropsLattice :: (Ord c, C.CRel c) => [C.Rop c]
ropsLattice = Op.ropList "lattice"  -- GROUP
    --   USAGE     , CONSTRUCTOR    , ATTRIBUTE
    [ ( "compose R", Op.consCompose , C.roaOne "-relmap" [] )
    , ( "join R"   , Op.consJoin    , C.roaOne "-relmap" [] )
    , ( "meet R"   , Op.consMeet    , C.roaOne "-relmap" [] )
    , ( "none R"   , Op.consNone    , C.roaOne "-relmap" [] )
    , ( "some R"   , Op.consSome    , C.roaOne "-relmap" [] )
    , ( "sub R"    , Op.consSub     , C.roaOne "-relmap" [] )
    ]


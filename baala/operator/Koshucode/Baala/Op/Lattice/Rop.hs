{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators on relational lattice.

module Koshucode.Baala.Op.Lattice.Rop
( latticeRops,
) where

import qualified Koshucode.Baala.Core                 as C
import qualified Koshucode.Baala.Op.Builtin           as Op
import qualified Koshucode.Baala.Op.Lattice.Confl     as Op
import qualified Koshucode.Baala.Op.Lattice.Restrict  as Op
import qualified Koshucode.Baala.Op.Lattice.Tropashko as Op


-- | Lattice implementations of relmap operators.
--
--   [@join@]    Join two relations.
--
--   [@maybe R@]
--     Meet input and given relation.
--     It keeps input tuples of which counterparts are totally negated.
-- 
--   [@meet@]    Meet two relations.
--
--   [@none@]    Restriction by relmaps.
--
--   [@some@]    Restriction by relmaps.
--
--   [@sub@]     Restriction to subrelation.
--
latticeRops :: (Ord c, C.CRel c, C.CNil c) => [C.Rop c]
latticeRops = Op.ropList "lattice"  -- GROUP
    --   USAGE     , CONSTRUCTOR    , ATTRIBUTE
    [ ( "both R"   , Op.consBoth    , C.roaOne "-relmap" [] )
    , ( "compose R", Op.consCompose , C.roaOne "-relmap" [] )
    , ( "equal"    , Op.consEqual   , C.roaOne "-relmap" [] )
    , ( "join R"   , Op.consJoin    , C.roaOne "-relmap" [] )
    , ( "maybe R"  , Op.consMaybe   , C.roaOne "-relmap" [] )
    , ( "meet R"   , Op.consMeet    , C.roaOne "-relmap" [] )
    , ( "none R"   , Op.consNone    , C.roaOne "-relmap" [] )
    , ( "some R"   , Op.consSome    , C.roaOne "-relmap" [] )
    , ( "sub R"    , Op.consSub     , C.roaOne "-relmap" [] )
    ]


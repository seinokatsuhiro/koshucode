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
    --         CONSTRUCTOR     USAGE        ATTRIBUTE
    [ Op.ropI  Op.consCompose  "compose R"  "-relmap/"
    , Op.ropI  Op.consJoin     "join R"     "-relmap/"
    , Op.ropI  Op.consMeet     "meet R"     "-relmap/"
    , Op.ropI  Op.consNone     "none R"     "-relmap/"
    , Op.ropI  Op.consSome     "some R"     "-relmap/"
    , Op.ropI  Op.consSub      "sub R"      "-relmap/"
    ]


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
    --         CONSTRUCTOR      USAGE          ATTRIBUTE
    [ Op.def  Op.consCompose   "compose R"    "1 -relmap/"
    , Op.def  Op.consJoin      "join R"       "1 -relmap/"
    , Op.def  Op.consMeet      "meet R"       "1 -relmap/"
    , Op.def  Op.consNone      "none R"       "1 -relmap/"
    , Op.def  Op.consSome      "some R"       "1 -relmap/"
    , Op.def  Op.consSomeMeet  "some-meet R"  "1 -relmap/"
    , Op.def  Op.consSub       "sub R"        "1 -relmap/"
    ]


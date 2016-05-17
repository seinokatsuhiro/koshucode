{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators on relational lattice.

module Koshucode.Baala.Rop.Flat.Lattice.Rop
  ( ropsLattice,
  ) where

import qualified Koshucode.Baala.Core                  as C
import qualified Koshucode.Baala.Rop.Base              as Op
import qualified Koshucode.Baala.Rop.Flat.Lattice.Restrict  as Op
import qualified Koshucode.Baala.Rop.Flat.Lattice.Tropashko as Op


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
ropsLattice :: (Ord c) => [C.Rop c]
ropsLattice = Op.ropList "lattice"  -- GROUP
    --         CONSTRUCTOR      USAGE          ATTRIBUTE
    [ Op.def  Op.consCompose   "compose R [-share /P ...]"  "-relmap/ . -share?"
    , Op.def  Op.consJoin      "join R [-share /P ...]"     "-relmap/ . -share?"
    , Op.def  Op.consMeet      "meet R [-share /P ...]"     "-relmap/ . -share?"
    , Op.def  Op.consNone      "none R"       "-relmap/"
    , Op.def  Op.consNoneMeet  "none-meet R"  "-relmap/"
    , Op.def  Op.consSome      "some R"       "-relmap/"
    , Op.def  Op.consSomeMeet  "some-meet R"  "-relmap/"
    , Op.def  Op.consSub       "sub R"        "-relmap/"
    ]


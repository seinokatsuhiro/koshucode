{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators on relational lattice.

module Koshucode.Baala.Rop.Flat.Lattice.Rop
  ( ropsLattice,
  ) where

import qualified Koshucode.Baala.Overture                   as O
import qualified Koshucode.Baala.Core                       as C
import qualified Koshucode.Baala.Rop.Base                   as Rop
import qualified Koshucode.Baala.Rop.Flat.Lattice.Restrict  as Rop
import qualified Koshucode.Baala.Rop.Flat.Lattice.Tropashko as Rop


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
ropsLattice = Rop.ropList "lattice"  -- GROUP
    --        CONSTRUCTOR         USAGE                              ATTRIBUTE
    [ Rop.rop Rop.consCompose   [ "compose R [-share /P ...]"    O.& "-relmap/ . -share?" ]
    , Rop.rop Rop.consJoin      [ "join R [-share /P ...]"       O.& "-relmap/ . -share?" ]
    , Rop.rop Rop.consMeet      [ "meet R [-share /P ...]"       O.& "-relmap/ . -share?" ]
    , Rop.rop Rop.consNone      [ "none R"                       O.& "-relmap/" ]
    , Rop.rop Rop.consNoneMeet  [ "none-meet R [-share /P ...]"  O.& "-relmap/ . -share?" ]
    , Rop.rop Rop.consSome      [ "some R"                       O.& "-relmap/" ]
    , Rop.rop Rop.consSomeMeet  [ "some-meet R [-share /P ...]"  O.& "-relmap/ . -share?" ]
    , Rop.rop Rop.consSub       [ "sub R [-share /P ...]"        O.& "-relmap/ . -share?" ]
    ]


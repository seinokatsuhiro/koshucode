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
ropsLattice = Rop.rops "lattice"
    [ Rop.consCompose   O.& [ "compose R [-share /P ...]"    O.& "-relmap/ . -share?" ]
    , Rop.consJoin      O.& [ "join R [-share /P ...]"       O.& "-relmap/ . -share?" ]
    , Rop.consMeet      O.& [ "meet R [-share /P ...]"       O.& "-relmap/ . -share?" ]
    , Rop.consNone      O.& [ "none R"                       O.& "-relmap/" ]
    , Rop.consNoneMeet  O.& [ "none-meet R [-share /P ...]"  O.& "-relmap/ . -share?" ]
    , Rop.consSome      O.& [ "some R"                       O.& "-relmap/" ]
    , Rop.consSomeMeet  O.& [ "some-meet R [-share /P ...]"  O.& "-relmap/ . -share?" ]
    , Rop.consSub       O.& [ "sub R [-share /P ...]"        O.& "-relmap/ . -share?" ]
    ]


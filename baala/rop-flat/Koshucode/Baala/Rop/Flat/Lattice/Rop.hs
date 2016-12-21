{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators on relational lattice.

module Koshucode.Baala.Rop.Flat.Lattice.Rop
  ( ropsLattice,
  ) where

import qualified Koshucode.Baala.DataPlus                   as K
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
    [ Rop.consCompose   K.& [ "compose R [-share /P ...]"    K.& "-relmap/ . -share?" ]
    , Rop.consJoin      K.& [ "join R [-share /P ...]"       K.& "-relmap/ . -share?" ]
    , Rop.consMeet      K.& [ "meet R [-share /P ...]"       K.& "-relmap/ . -share?" ]
    , Rop.consNone      K.& [ "none R"                       K.& "-relmap/" ]
    , Rop.consNoneMeet  K.& [ "none-meet R [-share /P ...]"  K.& "-relmap/ . -share?" ]
    , Rop.consSome      K.& [ "some R"                       K.& "-relmap/" ]
    , Rop.consSomeMeet  K.& [ "some-meet R [-share /P ...]"  K.& "-relmap/ . -share?" ]
    , Rop.consSub       K.& [ "sub R [-share /P ...]"        K.& "-relmap/ . -share?" ]
    ]


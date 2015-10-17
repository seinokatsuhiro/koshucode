{-# OPTIONS_GHC -Wall #-}

-- | Lattice relmap operators.

module Koshucode.Baala.Op.Lattice
  ( module Koshucode.Baala.Op.Lattice.Restrict,
    module Koshucode.Baala.Op.Lattice.Rop,
    module Koshucode.Baala.Op.Lattice.Tropashko,
    -- * Naming conventions
    -- $NamingConventions
  ) where

import Koshucode.Baala.Op.Lattice.Restrict
import Koshucode.Baala.Op.Lattice.Rop
import Koshucode.Baala.Op.Lattice.Tropashko


-- ----------------------
-- $NamingConventions
--
--  Functions for relation-mapping operators are
--  named under some conventions.
--
--  [@xxx-yyy@]       Name of relation-mapping operator.
--
--  [@consXxxYyy@]    Functions of 'Koshucode.Baala.Core.Relmap.Operator.RopCons'.
--                    These functions make 'Koshucode.Baala.Core.Relmap.Relmap.Relmap'
--                    from operator usages.
--
--  [@relmapXxxYyy@]  Functions from 'Koshucode.Baala.Core.Relmap.Relmap.Relmap'
--                    to 'Koshucode.Baala.Core.Relmap.Relmap.Relmap'.
--
--  [@relkitXxxYyy@]  Functions that make 'Koshucode.Baala.Core.Relmap.Relkit.Relkit'.


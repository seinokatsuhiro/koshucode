{-# OPTIONS_GHC -Wall #-}

-- | Lattice relmap operators.

module Koshucode.Baala.Rop.Lattice
  ( module Koshucode.Baala.Rop.Lattice.Restrict,
    module Koshucode.Baala.Rop.Lattice.Rop,
    module Koshucode.Baala.Rop.Lattice.Tropashko,
    -- * Naming conventions
    -- $NamingConventions
  ) where

import Koshucode.Baala.Rop.Lattice.Restrict
import Koshucode.Baala.Rop.Lattice.Rop
import Koshucode.Baala.Rop.Lattice.Tropashko


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


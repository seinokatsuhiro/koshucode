{-# OPTIONS_GHC -Wall #-}

{-| Minimal relation-mapping operators -}

module Koshucode.Baala.Op.Minimal
( -- * Relation-mapping operators
  module Koshucode.Baala.Op.Minimal.Origin,
  module Koshucode.Baala.Op.Minimal.Restrict,
  module Koshucode.Baala.Op.Minimal.Rop,
  module Koshucode.Baala.Op.Minimal.Term,
  module Koshucode.Baala.Op.Minimal.Tropashko,

  -- * Naming conventions
  -- $NamingConventions
) where

import Koshucode.Baala.Op.Minimal.Origin
import Koshucode.Baala.Op.Minimal.Restrict
import Koshucode.Baala.Op.Minimal.Rop
import Koshucode.Baala.Op.Minimal.Term
import Koshucode.Baala.Op.Minimal.Tropashko



-- ----------------------
{- $NamingConventions

   Functions for relation-mapping operators are
   named under some conventions.

   [@xxx@]         Name of relation-mapping operator.

   [@consXxx@]     Functions of 'Koshucode.Baala.Core.Relmap.Rop.RopCons'.
                   These functions make 'Koshucode.Baala.Core.Relmap.Relmap.Relmap'
                   from operator usages.

   [@relmapXxx@]   Functions from 'Koshucode.Baala.Core.Relmap.Relmap.Relmap'
                   to 'Koshucode.Baala.Core.Relmap.Relmap.Relmap'.

   [@relkitXxx@]   Functions that make 'Koshucode.Baala.Core.Relmap.Relkit.Relkit'.

-}


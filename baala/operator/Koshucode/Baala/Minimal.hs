{-# OPTIONS_GHC -Wall #-}

{-| Minimal relation-mapping operators -}

module Koshucode.Baala.Minimal
( -- * Relation-mapping operators
  module Koshucode.Baala.Minimal.Origin,
  module Koshucode.Baala.Minimal.Restrict,
  module Koshucode.Baala.Minimal.Rop,
  module Koshucode.Baala.Minimal.Term,
  module Koshucode.Baala.Minimal.Tropashko,

  -- * Naming conventions
  -- $NamingConventions
) where

import Koshucode.Baala.Minimal.Origin
import Koshucode.Baala.Minimal.Restrict
import Koshucode.Baala.Minimal.Rop
import Koshucode.Baala.Minimal.Term
import Koshucode.Baala.Minimal.Tropashko



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


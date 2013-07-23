{-# OPTIONS_GHC -Wall #-}

{-| Minimal relational operators. -}

module Koshucode.Baala.Minimal.Relmap
( -- * Implementation
  module Koshucode.Baala.Minimal.Relmap.Implement,
  module Koshucode.Baala.Minimal.Relmap.Operand,
  module Koshucode.Baala.Minimal.Relmap.Restrict,
  module Koshucode.Baala.Minimal.Relmap.Tropashko,
  module Koshucode.Baala.Minimal.Relmap.Unary,

  -- * Naming conventions
  -- $NamingConventions
) where

import Koshucode.Baala.Minimal.Relmap.Implement
import Koshucode.Baala.Minimal.Relmap.Operand
import Koshucode.Baala.Minimal.Relmap.Restrict
import Koshucode.Baala.Minimal.Relmap.Tropashko
import Koshucode.Baala.Minimal.Relmap.Unary



-- ----------------------
{- $NamingConventions

   Functions for relational mapping are
   named under some conventions.
   Relational mapping is a kind of functions
   that calculate relations from relations.

   [@xxx@]        Name of relational operator.

   [@relopXxx@]   Functions of 'Koshucode.Baala.Core.Relmap.Implement.Relop'.
                  These functions construct 'Koshucode.Baala.Base.Content.Relmap.Relmap'
                  from operator usages, but constructions are failed
                  if operators are misused.

   [@relmapXxx@]  Functions from 'Koshucode.Baala.Base.Content.Relmap.Relmap'
                  to 'Koshucode.Baala.Base.Content.Relmap.Relmap'.

   [@relXxx@]     Functions from 'Koshucode.Baala.Base.Data.Rel'
                  to 'Koshucode.Baala.Base.Data.Rel'.
                  These are basic functions for relational operators.

   [@likeXxx@]    Operand parser.

   [@LikeXxx@]    Operand pattern.

-}


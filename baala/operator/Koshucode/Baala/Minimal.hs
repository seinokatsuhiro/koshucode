{-# OPTIONS_GHC -Wall #-}

{-| Minimal relational operators -}

module Koshucode.Baala.Minimal
( -- * Relatoinal operators
  module Koshucode.Baala.Minimal.Operand,
  module Koshucode.Baala.Minimal.Operator,
  module Koshucode.Baala.Minimal.Restrict,
  module Koshucode.Baala.Minimal.Tropashko,
  module Koshucode.Baala.Minimal.Unary,

  -- * Naming conventions
  -- $NamingConventions
) where

import Koshucode.Baala.Minimal.Operand
import Koshucode.Baala.Minimal.Operator
import Koshucode.Baala.Minimal.Restrict
import Koshucode.Baala.Minimal.Tropashko
import Koshucode.Baala.Minimal.Unary



-- ----------------------
{- $NamingConventions

   Functions for relational mapping are
   named under some conventions.
   Relational mapping is a kind of functions
   that calculate relations from relations.

   [@xxx@]        Name of relational operator.

   [@ropConsXxx@] Functions of 'Koshucode.Baala.Core.Relmap.Implement.RopCons'.
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


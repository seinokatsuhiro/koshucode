{-# OPTIONS_GHC -Wall #-}

{-| Minimal relational operators -}

module Koshucode.Baala.Minimal
( -- * Relatoinal operators
  module Koshucode.Baala.Minimal.Operator,
  module Koshucode.Baala.Minimal.Origin,
  module Koshucode.Baala.Minimal.Restrict,
  module Koshucode.Baala.Minimal.Term,
  module Koshucode.Baala.Minimal.Tropashko,

  -- * Naming conventions
  -- $NamingConventions
) where

import Koshucode.Baala.Minimal.Operator
import Koshucode.Baala.Minimal.Origin
import Koshucode.Baala.Minimal.Restrict
import Koshucode.Baala.Minimal.Term
import Koshucode.Baala.Minimal.Tropashko



-- ----------------------
{- $NamingConventions

   Functions for relational mapping are
   named under some conventions.

   [@xxx@]        Name of relational operator.

   [@ropConsXxx@] Functions of 'Koshucode.Baala.Core.Relmap.Implement.RopCons'.
                  These functions construct 'Koshucode.Baala.Base.Content.Relmap.Relmap'
                  from operator usages, but constructions are failed
                  if operators are misused.

   [@relmapXxx@]  Functions from 'Koshucode.Baala.Base.Content.Relmap.Relmap'
                  to 'Koshucode.Baala.Base.Content.Relmap.Relmap'.

   [@relfyXxx@]   Functions that make 'Koshucode.Baala.Core.Relmap.Relfy'.

   [@likeXxx@]    Operand parser.

   [@LikeXxx@]    Operand pattern.

-}


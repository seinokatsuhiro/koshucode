{-# OPTIONS_GHC -Wall #-}

-- | Minimal implementations of relmaps

module Koshucode.Baala.Minimal.Operator
( -- * Implementation
  minimalRops

  -- * Operators
  -- $ListOfOperators
) where

import Koshucode.Baala.Core
import qualified Koshucode.Baala.Builtin as Kit
import Koshucode.Baala.Minimal.Operand
import Koshucode.Baala.Minimal.Restrict
import Koshucode.Baala.Minimal.Tropashko
import Koshucode.Baala.Minimal.Unary



-- ----------------------  Operators

{-| Minimal implementations of relmaps. -}
minimalRops :: (Ord c) => [Rop c]
minimalRops = Kit.operators "minimal"
    -- Relmap operators in alphabetical order
    [ o "cut"      LikePick     ropConsCut
    , o "empty"    LikeId       ropConsEmpty
    , o "id"       LikeId       ropConsId
    , o "join"     LikeMeet     ropConsJoin
    , o "meet"     LikeMeet     ropConsMeet
    , o "minus"    LikeMeet     ropConsMinus
    , o "pick"     LikePick     ropConsPick
    , o "reldee"   LikeId       ropConsReldee
    , o "reldum"   LikeId       ropConsReldum
    , o "rename"   LikeRename   ropConsRename
    , o "some"     LikeMeet     ropConsSome
    , o "source"   LikeSource   ropConsSource
    ] where o = (,,)



-- ----------------------
{- $ListOfOperators

   [@cut@]     Project relation to unspecified terms

   [@join@]    Calculate join of two relations.

   [@meet@]    Calculate meet of two relations.

   [@pick@]    Project relation to specified terms

   [@reldee@]  Nullary fullset relation

   [@reldum@]  Nullary empty relation

   [@rename@]  Change term name

   [@source@]  Read relation from data source

-}


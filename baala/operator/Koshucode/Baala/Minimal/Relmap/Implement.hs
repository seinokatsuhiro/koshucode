{-# OPTIONS_GHC -Wall #-}

-- | Minimal implementations of relmaps

module Koshucode.Baala.Minimal.Relmap.Implement
( -- * Implementation
  minimalOperators

  -- * Operators
  -- $ListOfOperators
) where

import Koshucode.Baala.Core
import qualified Koshucode.Baala.Builtin as Kit
import Koshucode.Baala.Minimal.Relmap.Operand
import Koshucode.Baala.Minimal.Relmap.Restrict
import Koshucode.Baala.Minimal.Relmap.Tropashko
import Koshucode.Baala.Minimal.Relmap.Unary



-- ----------------------  Operators

{-| Minimal implementations of relmaps. -}
minimalOperators :: (Ord c) => [OpImplement c]
minimalOperators = Kit.operators "minimal"
    -- Relmap operators in alphabetical order
    [ o "cut"      LikePick     relopCut
    , o "empty"    LikeId       relopEmpty
    , o "id"       LikeId       relopId
    , o "join"     LikeMeet     relopJoin
    , o "meet"     LikeMeet     relopMeet
    , o "minus"    LikeMeet     relopMinus
    , o "pick"     LikePick     relopPick
    , o "reldee"   LikeId       relopReldee
    , o "reldum"   LikeId       relopReldum
    , o "rename"   LikeRename   relopRename
    , o "some"     LikeMeet     relopSome
    , o "source"   LikeSource   relopSource
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


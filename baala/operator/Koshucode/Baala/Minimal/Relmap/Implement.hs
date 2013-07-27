{-# OPTIONS_GHC -Wall #-}

-- | Minimal implementations of relmaps

module Koshucode.Baala.Minimal.Relmap.Implement
( -- * Implementation
  minimalOperators

  -- * Operators
  -- $ListOfOperators
) where

import Koshucode.Baala.Base
import Koshucode.Baala.Core
import qualified Koshucode.Baala.Builtin as Kit

import Koshucode.Baala.Minimal.Relmap.Operand
import Koshucode.Baala.Minimal.Relmap.Restrict
import Koshucode.Baala.Minimal.Relmap.Tropashko
import Koshucode.Baala.Minimal.Relmap.Unary



-- ----------------------  Operators

{-| Minimal implementations of relmaps. -}
minimalOperators :: (Ord c) => [OpImplement c]
minimalOperators = Kit.builtinOperators ++ Kit.operators
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

relopSource :: Relop c
relopSource use =
  do sign <- Kit.getWord  use "-sign"
     ns   <- Kit.getTerms use "-term"
     Right $ relmapSource use sign ns

relopReldee, relopReldum :: Relop c
relopReldee use = Right $ relmapConst use "reldee" reldee
relopReldum use = Right $ relmapConst use "reldee" reldum



-- ----------------------
{- $ListOfOperators

   [@cut@] Project relation to unspecified terms

   [@join@] Calculate join of two relations.

   [@meet@] Calculate meet of two relations.

   [@pick@] Project relation to specified terms

   [@reldee@] Nullary fullset relation

   [@reldum@] Nullary empty relation

   [@rename@] Change term name

   [@source@] Read relation from data source

-}


{-# OPTIONS_GHC -Wall #-}

-- | Minimal implementations of relmaps

module Koshucode.Baala.Minimal.Relmap.Implement
( -- * Implementation
  minimalOperators

  -- * Operators
  -- $ListOfOperators
) where

import Koshucode.Baala.Minimal.OpKit as Kit

import Koshucode.Baala.Minimal.Relmap.Operand
import Koshucode.Baala.Minimal.Relmap.Restrict
import Koshucode.Baala.Minimal.Relmap.Tropashko
import Koshucode.Baala.Minimal.Relmap.Unary



-- ----------------------  Operators

builtinOperators :: (Ord c) => [OpImplement c]
builtinOperators = operators [ ("|", LikeEmpty, consConcat) ]

consConcat :: Relop c
consConcat = Right . mconcat . opSubmap

{-| Minimal implementations of relmaps. -}
minimalOperators :: (Ord c) => [OpImplement c]
minimalOperators = builtinOperators ++ operators
    -- Relmap operators in alphabetical order
    [ o "cut"      LikePick     relopCut
    , o "empty"    LikeEmpty    relopEmpty
    , o "id"       LikeEmpty    relopId
    , o "join"     LikeMeet     relopJoin
    , o "meet"     LikeMeet     relopMeet
    , o "minus"    LikeMeet     relopMinus
    , o "pick"     LikePick     relopPick
    , o "reldee"   LikeEmpty    relopReldee
    , o "reldum"   LikeEmpty    relopReldum
    , o "rename"   LikeRename   relopRename
    , o "some"     LikeMeet     relopSome
    , o "source"   LikeSource   relopSource
    ] where o = (,,)

relopSource :: Relop c
relopSource use =
  do sign <- getWord  use "-sign"
     ns   <- getTerms use "-term"
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


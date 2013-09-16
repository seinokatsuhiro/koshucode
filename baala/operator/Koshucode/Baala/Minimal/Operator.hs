{-# OPTIONS_GHC -Wall #-}

-- | Minimal implementations of relmaps

module Koshucode.Baala.Minimal.Operator
( -- * Implementation
  minimalRops

  -- * Operators
  -- $ListOfOperators
) where

import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin
import Koshucode.Baala.Minimal.Operand
import Koshucode.Baala.Minimal.Restrict
import Koshucode.Baala.Minimal.Tropashko
import Koshucode.Baala.Minimal.Unary



-- ----------------------  Operators

{-| Minimal implementations of relmaps. -}
minimalRops :: (Ord c) => [C.Rop c]
minimalRops = Builtin.ropList "minimal"
    [ o "cut /N ..."         LikePick     ropConsCut
    , o "empty"              LikeId       ropConsEmpty
    , o "id"                 LikeId       ropConsId
    , o "join R"             LikeMeet     ropConsJoin
    , o "meet R"             LikeMeet     ropConsMeet
    , o "none R"             LikeMeet     ropConsNone
    , o "pick /N ..."        LikePick     ropConsPick
    , o "reldee"             LikeId       ropConsReldee
    , o "reldum"             LikeId       ropConsReldum
    , o "rename /N /N ..."   LikePick     ropConsRename
    , o "some R"             LikeMeet     ropConsSome
    , o "source P /N ..."    LikeSource   ropConsSource
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


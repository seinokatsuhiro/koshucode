{-# OPTIONS_GHC -Wall #-}

{-| Minimal implementations of relmaps -}

module Koshucode.Baala.Minimal.Operator
( -- * Implementation
  minimalRops

  -- * Operators
  -- $ListOfOperators
) where

import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Builtin
import qualified Koshucode.Baala.Minimal.Operand   as Mini
import qualified Koshucode.Baala.Minimal.Origin    as Mini
import qualified Koshucode.Baala.Minimal.Restrict  as Mini
import qualified Koshucode.Baala.Minimal.Term      as Mini
import qualified Koshucode.Baala.Minimal.Tropashko as Mini



-- ----------------------  Operators

{-| Minimal implementations of relmaps. -}
minimalRops :: (Ord c) => [C.Rop c]
minimalRops = Builtin.ropList "minimal"
    [ o "contents /N"        Mini.LikePick     Mini.ropConsContents
    , o "cut /N ..."         Mini.LikePick     Mini.ropConsCut
    , o "empty"              Mini.LikeId       Mini.ropConsEmpty
    , o "id"                 Mini.LikeId       Mini.ropConsId
    , o "join R"             Mini.LikeMeet     Mini.ropConsJoin
    , o "meet R"             Mini.LikeMeet     Mini.ropConsMeet
    , o "none R"             Mini.LikeMeet     Mini.ropConsNone
    , o "pick /N ..."        Mini.LikePick     Mini.ropConsPick
    , o "reldee"             Mini.LikeId       Mini.ropConsReldee
    , o "reldum"             Mini.LikeId       Mini.ropConsReldum
    , o "rename /N /N ..."   Mini.LikePick     Mini.ropConsRename
    , o "some R"             Mini.LikeMeet     Mini.ropConsSome
    , o "source P /N ..."    Mini.LikeSource   Mini.ropConsSource
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


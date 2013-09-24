{-# OPTIONS_GHC -Wall #-}

{-| Minimal implementations of relmap operators. -}

module Koshucode.Baala.Minimal.Operator
( minimalRops
  -- $ListOfOperators
) where

import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Builtin
import qualified Koshucode.Baala.Minimal.Operand   as Rop
import qualified Koshucode.Baala.Minimal.Origin    as Rop
import qualified Koshucode.Baala.Minimal.Restrict  as Rop
import qualified Koshucode.Baala.Minimal.Term      as Rop
import qualified Koshucode.Baala.Minimal.Tropashko as Rop

{-| Minimal implementations of relmap operators. -}
minimalRops :: (Ord c) => [C.Rop c]
minimalRops = Builtin.ropList "minimal"  -- GROUP
    [ o "contents /N"        Rop.LikePick     Rop.ropConsContents
    , o "cut /N ..."         Rop.LikePick     Rop.ropConsCut
    , o "empty"              Rop.LikeId       Rop.ropConsEmpty
    , o "id"                 Rop.LikeId       Rop.ropConsId
    , o "join R"             Rop.LikeMeet     Rop.ropConsJoin
    , o "meet R"             Rop.LikeMeet     Rop.ropConsMeet
    , o "none R"             Rop.LikeMeet     Rop.ropConsNone
    , o "pick /N ..."        Rop.LikePick     Rop.ropConsPick
    , o "reldee"             Rop.LikeId       Rop.ropConsReldee
    , o "reldum"             Rop.LikeId       Rop.ropConsReldum
    , o "rename /N /N ..."   Rop.LikePick     Rop.ropConsRename
    , o "some R"             Rop.LikeMeet     Rop.ropConsSome
    , o "source P /N ..."    Rop.LikeSource   Rop.ropConsSource
    --   SYNOPSIS            OPERAND          CONSTRUCTOR
    ] where o = (,,)



-- ----------------------
{- $ListOfOperators

   [@cut@]       Project relation to unspecified terms.

   [@contents@]  Make nary relation of all contetnts.

   [@id@]        Identity relmap.

   [@join@]      Calculate join of two relations.

   [@meet@]      Calculate meet of two relations.

   [@none@]      Restriction by relmaps.

   [@pick@]      Project relation to specified terms.

   [@reldee@]    Nullary full relation.

   [@reldum@]    Nullary empty relation.

   [@rename@]    Change term name.

   [@some@]      Restriction by relmaps.

   [@source@]    Read relation from data source.

-}


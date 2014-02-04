{-# OPTIONS_GHC -Wall #-}

{-| Minimal implementations of relmap operators. -}

module Koshucode.Baala.Minimal.Rop
( minimalRops,
  -- $ListOfOperators
) where

import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Builtin
import qualified Koshucode.Baala.Minimal.Origin    as Rop
import qualified Koshucode.Baala.Minimal.Restrict  as Rop
import qualified Koshucode.Baala.Minimal.Term      as Rop
import qualified Koshucode.Baala.Minimal.Tropashko as Rop

{-| Minimal implementations of relmap operators. -}
minimalRops :: (Ord c) => [C.Rop c]
minimalRops = Builtin.ropList "minimal"  -- GROUP
    [ ( "contents /N"      , Rop.consContents , C.sortList "-term" [] )
    , ( "cut /P ..."       , Rop.consCut      , C.sortList "-term" [] )
    , ( "empty"            , Rop.consEmpty    , C.sortNone [] )
    , ( "id"               , Rop.consId       , C.sortNone [] )
    , ( "join R"           , Rop.consJoin     , C.sortOne  "-relmap" [] )
    , ( "meet R"           , Rop.consMeet     , C.sortOne  "-relmap" [] )
    , ( "none R"           , Rop.consNone     , C.sortOne  "-relmap" [] )
    , ( "pick /P ..."      , Rop.consPick     , C.sortList "-term"   [] )
    , ( "reldee"           , Rop.consReldee   , C.sortNone [] )
    , ( "reldum"           , Rop.consReldum   , C.sortNone [] )
    , ( "rename /N /P ..." , Rop.consRename   , C.sortList "-term"   [] )
    , ( "some R"           , Rop.consSome     , C.sortOne  "-relmap" [] )
    , ( "source P /T ..."  , Rop.consSource   , C.sortOneList "-pattern" "-term" [] )
    , ( "sub R"            , Rop.consSub      , C.sortOne  "-relmap" [] )
    --   USAGE               CONSTRUCTOR        OPERAND
    ]

-- ----------------------
{- $ListOfOperators

   [@contents@]  Make nary relation of all contetnts.

   [@cut@]       Project relation to unspecified terms.

   [@id@]        Identity relmap.

   [@join@]      Join two relations.

   [@meet@]      Meet two relations.

   [@none@]      Restriction by relmaps.

   [@pick@]      Project relation to specified terms.

   [@reldee@]    Nullary full relation.

   [@reldum@]    Nullary empty relation.

   [@rename@]    Change term name.

   [@some@]      Restriction by relmaps.

   [@source@]    Read relation from data source.

   [@sub@]       Restriction to subrelation.

-}


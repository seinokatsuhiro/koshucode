{-# OPTIONS_GHC -Wall #-}

{-| Minimal implementations of relmap operators. -}

module Koshucode.Baala.Op.Minimal.Rop
( minimalRops,
  -- $ListOfOperators
) where

import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Op.Builtin           as Builtin
import qualified Koshucode.Baala.Op.Minimal.Origin    as Op
import qualified Koshucode.Baala.Op.Minimal.Restrict  as Op
import qualified Koshucode.Baala.Op.Minimal.Term      as Op
import qualified Koshucode.Baala.Op.Minimal.Tropashko as Op

{-| Minimal implementations of relmap operators. -}
minimalRops :: (Ord c) => [C.Rop c]
minimalRops = Builtin.ropList "minimal"  -- GROUP
    [ ( "contents /N"      , Op.consContents , C.sortList "-term" [] )
    , ( "cut /P ..."       , Op.consCut      , C.sortList "-term" [] )
    , ( "empty"            , Op.consEmpty    , C.sortNone [] )
    , ( "equal"            , Op.consEqual    , C.sortOne  "-relmap" [] )
    , ( "id"               , Op.consId       , C.sortNone [] )
    , ( "join R"           , Op.consJoin     , C.sortOne  "-relmap" [] )
    , ( "meet R"           , Op.consMeet     , C.sortOne  "-relmap" [] )
    , ( "none R"           , Op.consNone     , C.sortOne  "-relmap" [] )
    , ( "pick /P ..."      , Op.consPick     , C.sortList "-term"   [] )
    , ( "reldee"           , Op.consReldee   , C.sortNone [] )
    , ( "reldum"           , Op.consReldum   , C.sortNone [] )
    , ( "rename /N /P ..." , Op.consRename   , C.sortList "-term"   [] )
    , ( "some R"           , Op.consSome     , C.sortOne  "-relmap" [] )
    , ( "source P /T ..."  , Op.consSource   , C.sortOneList "-pattern" "-term" [] )
    , ( "sub R"            , Op.consSub      , C.sortOne  "-relmap" [] )
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


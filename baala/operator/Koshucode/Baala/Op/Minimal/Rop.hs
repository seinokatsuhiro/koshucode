{-# OPTIONS_GHC -Wall #-}

-- | Minimal implementations of relmap operators.

module Koshucode.Baala.Op.Minimal.Rop
( minimalRops,
  -- $ListOfOperators
) where

import qualified Koshucode.Baala.Core                 as C
import qualified Koshucode.Baala.Op.Builtin           as Op
import qualified Koshucode.Baala.Op.Minimal.Origin    as Op
import qualified Koshucode.Baala.Op.Minimal.Restrict  as Op
import qualified Koshucode.Baala.Op.Minimal.Term      as Op
import qualified Koshucode.Baala.Op.Minimal.Tropashko as Op


-- | Minimal implementations of relmap operators.
minimalRops :: (Ord c) => [C.Rop c]
minimalRops = Op.ropList "minimal"  -- GROUP
    [ ( "contents /N"      , Op.consContents , C.sortList "-term" [] )
    , ( "cut /P ..."       , Op.consCut      , C.sortList "-term" [] )
    , ( "cut-term /R"      , Op.consCutTerm  , C.sortOne  "-relmap" [] )
    , ( "dee"              , Op.consDee      , C.sortNone [] )
    , ( "dum"              , Op.consDum      , C.sortNone [] )
    , ( "empty"            , Op.consEmpty    , C.sortNone [] )
    , ( "equal"            , Op.consEqual    , C.sortOne  "-relmap" [] )
    , ( "id"               , Op.consId       , C.sortNone [] )
    , ( "join R"           , Op.consJoin     , C.sortOne  "-relmap" [] )
    , ( "meet R"           , Op.consMeet     , C.sortOne  "-relmap" [] )
    , ( "none R"           , Op.consNone     , C.sortOne  "-relmap" [] )
    , ( "pick /P ..."      , Op.consPick     , C.sortList "-term"   [] )
    , ( "pick-term /R"     , Op.consPickTerm , C.sortOne  "-relmap" [] )
    , ( "reldee"           , Op.consDee      , C.sortNone [] )
    , ( "reldum"           , Op.consDum      , C.sortNone [] )
    , ( "rename /N /P ..." , Op.consRename   , C.sortList "-term"   [] )
    , ( "some R"           , Op.consSome     , C.sortOne  "-relmap" [] )
    , ( "source P /T ..."  , Op.consSource   , C.sortOneList "-pattern" "-term" [] )
    , ( "sub R"            , Op.consSub      , C.sortOne  "-relmap" [] )
    --   USAGE               CONSTRUCTOR       OPERAND
    ]


-- ----------------------
-- $ListOfOperators
--
--  [@contents@]   Make nary relation of all contetnts.
--
--  [@cut@]        Project relation to unspecified terms.
--
--  [@cut-term@]   Project relation to terms not in relmap output.
--
--  [@id@]         Identity relmap.
--
--  [@join@]       Join two relations.
--
--  [@meet@]       Meet two relations.
--
--  [@none@]       Restriction by relmaps.
--
--  [@pick@]       Project relation to specified terms.
--
--  [@pick-term@]  Project relation to terms in relmap output.
--
--  [@dee@]        Nullary full relation.
--
--  [@dum@]        Nullary empty relation.
--
--  [@rename@]     Change term name.
--
--  [@some@]       Restriction by relmaps.
--
--  [@source@]     Read relation from data source.
--
--  [@sub@]        Restriction to subrelation.


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
    [ ( "contents /N"       , Op.consContents   , C.rodList "-term" [] )
    , ( "cut /P ..."        , Op.consCut        , C.rodList "-term" [] )
    , ( "cut-term /R"       , Op.consCutTerm    , C.rodOne  "-relmap" [] )
    , ( "dee"               , Op.consDee        , C.rodNone [] )
    , ( "dum"               , Op.consDum        , C.rodNone [] )
    , ( "empty"             , Op.consEmpty      , C.rodNone [] )
    , ( "equal"             , Op.consEqual      , C.rodOne  "-relmap" [] )
    , ( "id"                , Op.consId         , C.rodNone [] )
    , ( "join R"            , Op.consJoin       , C.rodOne  "-relmap" [] )
    , ( "meet R"            , Op.consMeet       , C.rodOne  "-relmap" [] )
    , ( "none R"            , Op.consNone       , C.rodOne  "-relmap" [] )
    , ( "pick /P ..."       , Op.consPick       , C.rodList "-term"   [] )
    , ( "pick-term /R"      , Op.consPickTerm   , C.rodOne  "-relmap" [] )
    , ( "rename /N /P ..."  , Op.consRename     , C.rodList "-term"   [] )
    , ( "move /P ... -to /N ...", Op.consMove   , C.rodList "-term" ["-to"] )
    , ( "some R"            , Op.consSome       , C.rodOne  "-relmap" [] )
    , ( "source P /T ..."   , Op.consSource     , C.rodOneList "-pattern" "-term" [] )
    , ( "source-term P R"   , Op.consSourceTerm , C.rodTwo "-pattern" "-relmap" [] )
    , ( "sub R"             , Op.consSub        , C.rodOne  "-relmap" [] )
    --   USAGE                CONSTRUCTOR         OPERAND
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
--  [@rehead@]     Change heading.
--
--  [@some@]       Restriction by relmaps.
--
--  [@source@]     Read relation from data source.
--
--  [@sub@]        Restriction to subrelation.


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
    [ ( "contents /N"       , Op.consContents   , C.roaList "-term" [] )
    , ( "cut /P ..."        , Op.consCut        , C.roaList "-term" [] )
    , ( "cut-term /R"       , Op.consCutTerm    , C.roaOne  "-relmap" [] )
    , ( "empty"             , Op.consEmpty      , C.roaNone [] )
    , ( "equal"             , Op.consEqual      , C.roaOne  "-relmap" [] )
    , ( "id"                , Op.consId         , C.roaNone [] )
    , ( "join R"            , Op.consJoin       , C.roaOne  "-relmap" [] )
    , ( "meet R"            , Op.consMeet       , C.roaOne  "-relmap" [] )
    , ( "none R"            , Op.consNone       , C.roaOne  "-relmap" [] )
    , ( "pick /P ..."       , Op.consPick       , C.roaList "-term"   [] )
    , ( "pick-term /R"      , Op.consPickTerm   , C.roaOne  "-relmap" [] )
    , ( "rename /N /P ..."  , Op.consRename     , C.roaList "-term"   [] )
    , ( "move /P ... -to /N ...", Op.consMove   , C.roaList "-term" ["-to"] )
    , ( "some R"            , Op.consSome       , C.roaOne  "-relmap" [] )
    , ( "sub R"             , Op.consSub        , C.roaOne  "-relmap" [] )
    --   USAGE                CONSTRUCTOR         ATTRIBUTE
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
--  [@rename@]     Change term name.
--
--  [@rehead@]     Change heading.
--
--  [@some@]       Restriction by relmaps.
--
--  [@sub@]        Restriction to subrelation.


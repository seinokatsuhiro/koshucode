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
import qualified Koshucode.Baala.Op.Minimal.Tropashko as Op


-- | Minimal implementations of relmap operators.
minimalRops :: (Ord c) => [C.Rop c]
minimalRops = Op.ropList "minimal"  -- GROUP
    [ ( "contents /N"       , Op.consContents   , C.roaList "-term" [] )
    , ( "empty"             , Op.consEmpty      , C.roaNone [] )
    , ( "equal"             , Op.consEqual      , C.roaOne  "-relmap" [] )
    , ( "id"                , Op.consId         , C.roaNone [] )
    , ( "join R"            , Op.consJoin       , C.roaOne  "-relmap" [] )
    , ( "meet R"            , Op.consMeet       , C.roaOne  "-relmap" [] )
    , ( "none R"            , Op.consNone       , C.roaOne  "-relmap" [] )
    , ( "some R"            , Op.consSome       , C.roaOne  "-relmap" [] )
    , ( "sub R"             , Op.consSub        , C.roaOne  "-relmap" [] )
    --   USAGE                CONSTRUCTOR         ATTRIBUTE
    ]


-- ----------------------
-- $ListOfOperators
--
--  [@contents@]   Make nary relation of all contetnts.
--
--  [@id@]         Identity relmap.
--
--  [@join@]       Join two relations.
--
--  [@meet@]       Meet two relations.
--
--  [@none@]       Restriction by relmaps.
--
--  [@some@]       Restriction by relmaps.
--
--  [@sub@]        Restriction to subrelation.


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
    [ ( "contents /N"      , Rop.consContents , C.operandList "-term" [] )
    , ( "cut /N ..."       , Rop.consCut      , C.operandList "-term" [] )
    , ( "empty"            , Rop.consEmpty    , C.operandNone [] )
    , ( "id"               , Rop.consId       , C.operandNone [] )
    , ( "join R"           , Rop.consJoin     , C.operandOne  "-relmap" [] )
    , ( "meet R"           , Rop.consMeet     , C.operandOne  "-relmap" [] )
    , ( "none R"           , Rop.consNone     , C.operandOne  "-relmap" [] )
    , ( "pick /N ..."      , Rop.consPick     , C.operandList "-term"   [] )
    , ( "reldee"           , Rop.consReldee   , C.operandNone [] )
    , ( "reldum"           , Rop.consReldum   , C.operandNone [] )
    , ( "rename /N /N ..." , Rop.consRename   , C.operandList "-term"   [] )
    , ( "some R"           , Rop.consSome     , C.operandOne  "-relmap" [] )
    , ( "source P /N ..."  , Rop.consSource   , C.operandOneList "-pattern" "-term" [] )
    , ( "sub R"            , Rop.consSub      , C.operandOne  "-relmap" [] )
    --   SYNOPSIS            CONSTRUCTOR        OPERAND
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


{-# OPTIONS_GHC -Wall #-}

{-| Minimal implementations of relmap operators. -}

module Koshucode.Baala.Minimal.Operator
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
    [ ( "contents /N"      , Rop.ropConsContents , C.operandList "-term" [] )
    , ( "cut /N ..."       , Rop.ropConsCut      , C.operandList "-term" [] )
    , ( "empty"            , Rop.ropConsEmpty    , C.operandNone [] )
    , ( "id"               , Rop.ropConsId       , C.operandNone [] )
    , ( "join R"           , Rop.ropConsJoin     , C.operandOne  "-relmap" [] )
    , ( "meet R"           , Rop.ropConsMeet     , C.operandOne  "-relmap" [] )
    , ( "none R"           , Rop.ropConsNone     , C.operandOne  "-relmap" [] )
    , ( "pick /N ..."      , Rop.ropConsPick     , C.operandList "-term"   [] )
    , ( "reldee"           , Rop.ropConsReldee   , C.operandNone [] )
    , ( "reldum"           , Rop.ropConsReldum   , C.operandNone [] )
    , ( "rename /N /N ..." , Rop.ropConsRename   , C.operandList "-term"   [] )
    , ( "some R"           , Rop.ropConsSome     , C.operandOne  "-relmap" [] )
    , ( "source P /N ..."  , Rop.ropConsSource   , C.operandOneList "-pattern" "-term" [] )
    , ( "sub R"            , Rop.ropConsSub      , C.operandOne  "-relmap" [] )
    --   SYNOPSIS            CONSTRUCTOR           OPERAND
    ]

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


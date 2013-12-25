{-# OPTIONS_GHC -Wall #-}

{-| Vanilla relational operators. -}

module Koshucode.Baala.Vanilla.Relmap.Operator
( vanillaRops,
  -- $Operators
) where

import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop

import qualified Koshucode.Baala.Vanilla.Relmap.Calc    as Rop
import qualified Koshucode.Baala.Vanilla.Relmap.Naming  as Rop
import qualified Koshucode.Baala.Vanilla.Relmap.Binary  as Rop
import qualified Koshucode.Baala.Vanilla.Relmap.Unary   as Rop
import Koshucode.Baala.Vanilla.Type

{-| Implementation of relational operators. -}
vanillaRops :: [C.Rop VContent]
vanillaRops = Rop.ropList "vanilla"
    [ ( "add /N E ..."         , Rop.ropConsAdd           , C.operandList    "-term"   [] )
    , ( "check-term /N ..."    , Rop.ropConsCheckTerm     , C.operandList    "-term"   ["-just", "-has", "-but"] )
    , ( "duplicate /N ..."     , Rop.ropConsDuplicate     , C.operandList    "-term"   [] )
    , ( "enclose /N"           , Rop.ropConsEnclose       , C.operandOne     "-term"   [] )
    , ( "full R ..."           , Rop.ropConsFull          , C.operandOne     "-relmap" [] )
    , ( "group /N R"           , Rop.ropConsGroup         , C.operandTwo     "-term" "-relmap" [] )
    , ( "hold E"               , Rop.ropConsHold          , C.operandList    "-term"   [] )
    , ( "maybe R"              , Rop.ropConsMaybe         , C.operandOne     "-relmap" [] )
    , ( "member /N /N"         , Rop.ropConsMember        , C.operandEnum    ["-1", "-2"] [] )
    , ( "prefix /P /N ..."     , Rop.ropConsPrefix        , C.operandOneList "-prefix" "-term" [] )
    , ( "prefix-change /P /Q"  , Rop.ropConsPrefixChange  , C.operandTwo     "-new" "-old" [] )
    , ( "range"                , Rop.ropConsRange         , C.operandOne     "-term"   ["-from", "-to"] )
    , ( "rank"                 , Rop.ropConsRank          , C.operandOne     "-add"    ["-order"] )
    , ( "rdf P /S /O"          , Rop.ropConsRdf           , C.operandOneList "-pattern" "-term" [] )
    , ( "size /N"              , Rop.ropConsSize          , C.operandOne     "-term"   [] )
    , ( "typename"             , Rop.ropConsTypename      , C.operandList    "-term"   [] )
    , ( "unprefix /P"          , Rop.ropConsUnprefix      , C.operandOne     "-prefix" [] )
    --  SYNOPSIS                 CONSTRUCTOR                OPERAND
    ]

-- ----------------------
{- $Operators

   [@add@]
  
   [@conf@]
  
   [@enclose@]
  
   [@group@]
  
   [@hold@]
  
   [@maybe@]
  
   [@maybe-both@]
  
   [@prefix@]
  
   [@prefix-change@]
  
   [@rdf@]
  
   [@size@]
  
   [@unhold@]
  
   [@unprefix@]

-}


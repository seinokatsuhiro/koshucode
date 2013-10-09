{-# OPTIONS_GHC -Wall #-}

{-| Vanilla relational operators. -}

module Koshucode.Baala.Vanilla.Relmap.Operator
( vanillaRops,
  -- $Operators
) where

import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin

import Koshucode.Baala.Vanilla.Relmap.Calc
import Koshucode.Baala.Vanilla.Relmap.Naming
import Koshucode.Baala.Vanilla.Relmap.Binary
import Koshucode.Baala.Vanilla.Relmap.Unary
import Koshucode.Baala.Vanilla.Type

{-| Implementation of relational operators. -}
vanillaRops :: [C.Rop VContent]
vanillaRops = Builtin.ropList "vanilla"
    [ ( "add /N E ..."         , ropConsAdd           , C.operandElems  "-term"   [])
    , ( "duplicate /N ..."     , ropConsDuplicate     , C.operandElems  "-term"   [])
    , ( "enclose /N"           , ropConsEnclose       , C.operandUnary  "-term"   [])
    , ( "full R ..."           , ropConsFull          , C.operandUnary  "-relmap" [])
    , ( "group /N R"           , ropConsGroup         , C.operandBinary "-term" "-relmap" [])
    , ( "hold E"               , ropConsHold          , C.operandElems  "-term"   [])
    , ( "maybe R"              , ropConsMaybe         , C.operandUnary  "-relmap" [])
    , ( "member /N /N"         , ropConsMember        , C.operandEnum   ["-1", "-2"] [])
    , ( "prefix /P /N ..."     , ropConsPrefix        , C.operandUncons "-prefix" "-term" [])
    , ( "prefix-change /P /Q"  , ropConsPrefixChange  , C.operandBinary "-new" "-old" [])
    , ( "range"                , ropConsRange         , C.operandUnary  "-term"   [])
    , ( "rank"                 , ropConsRank          , C.operandUnary  "-add"    ["-order"])
    , ( "rdf P /S /O"          , ropConsRdf           , C.operandUncons "-sign" "-term" [])
    , ( "size /N"              , ropConsSize          , C.operandUnary  "-term"   [])
    , ( "typename"             , ropConsTypename      , C.operandElems  "-term"   [])
    , ( "unprefix /P"          , ropConsUnprefix      , C.operandUnary  "-prefix" [])
    --  SYNOPSIS                 CONSTRUCTOR            OPERAND
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


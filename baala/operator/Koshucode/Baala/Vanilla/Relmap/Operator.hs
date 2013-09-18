{-# OPTIONS_GHC -Wall #-}

{-| Vanilla relational operators. -}

module Koshucode.Baala.Vanilla.Relmap.Operator
( vanillaRops

  -- * Operators
  -- $Operators
) where

import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin

import Koshucode.Baala.Vanilla.Relmap.Calc
import Koshucode.Baala.Vanilla.Relmap.Naming
import Koshucode.Baala.Vanilla.Relmap.Operand
import Koshucode.Baala.Vanilla.Relmap.Binary
import Koshucode.Baala.Vanilla.Relmap.Unary
import Koshucode.Baala.Vanilla.Type



-- ----------------------  Operators

{-| Implementation of relational operators. -}
vanillaRops :: [C.Rop VContent]
vanillaRops = Builtin.ropList "vanilla"
    [ o "add /N E ..."         LikeVal           ropConsAdd
    , o "enclose /N"           LikeSize          ropConsEnclose
    , o "full R ..."           LikeMeet          ropConsFull
    , o "group /N R"           LikeGroup         ropConsGroup
    , o "hold E"               LikeHold          ropConsHold
    , o "maybe R"              LikeMeet          ropConsMaybe
    , o "member /N /N"         LikePos           ropConsMember
    , o "prefix /P /N ..."     LikePrefix        ropConsPrefix
    , o "prefix-change /P /Q"  LikePrefixChange  ropConsPrefixChange
    , o "range"                LikeSize          ropConsRange
    , o "rank"                 LikeId            ropConsRank
    , o "rdf P /S /O"          LikeSource        ropConsRdf
    , o "size /N"              LikeSize          ropConsSize
    , o "typename"             LikeHold          ropConsTypename
    , o "unprefix /P"          LikeUnprefix      ropConsUnprefix
    ] where o = (,,)



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


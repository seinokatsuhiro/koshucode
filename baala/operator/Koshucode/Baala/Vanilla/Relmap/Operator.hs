{-# OPTIONS_GHC -Wall #-}

{-| Content formula. -}

module Koshucode.Baala.Vanilla.Relmap.Operator
( vanillaRops

  -- * Operators
  -- $Operators
) where

import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Kit

import Koshucode.Baala.Vanilla.Relmap.Calc
import Koshucode.Baala.Vanilla.Relmap.Naming
import Koshucode.Baala.Vanilla.Relmap.Operand
import Koshucode.Baala.Vanilla.Relmap.Binary
import Koshucode.Baala.Vanilla.Relmap.Unary
import Koshucode.Baala.Vanilla.Type



-- ----------------------  Operators

{-| Implementation of relational operators. -}
vanillaRops :: [C.Rop VContent]
vanillaRops = Kit.ropGroup "vanilla"
    -- Relmap operators in alphabetical order
    [ o "add"            LikeVal           ropConsAdd
    , o "conf"           LikeSize          ropConsConf
    , o "enclose"        LikeSize          ropConsEnclose
    , o "hang"           LikeMeet          ropConsHang
    , o "hold"           LikeHold          ropConsHold
    , o "maybe"          LikeMeet          ropConsMaybe
    , o "maybe-both"     LikeMeet          ropConsMaybeBoth
    , o "prefix"         LikePrefix        ropConsPrefix
    , o "prefix-change"  LikePrefixChange  ropConsPrefixChange
    , o "range"          LikeSize          ropConsRange
    , o "rank"           LikeId            ropConsRank
    , o "rdf"            LikeSource        ropConsRdf
    , o "size"           LikeSize          ropConsSize
    , o "typename"       LikeHold          ropConsTypename
    , o "unprefix"       LikeUnprefix      ropConsUnprefix
    ] where o = (,,)



-- ----------------------
-- $Operators
--
-- [@add@]
--
-- [@conf@]
--
-- [@enclose@]
--
-- [@hang@]
--
-- [@hold@]
--
-- [@maybe@]
--
-- [@maybe-both@]
--
-- [@prefix@]
--
-- [@prefix-change@]
--
-- [@rdf@]
--
-- [@size@]
--
-- [@unhold@]
--
-- [@unprefix@]
--


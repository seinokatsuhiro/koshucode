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
vanillaRops = Kit.operators "vanilla"
    -- Relmap operators in alphabetical order
    [ o "add"            LikeVal           relopAdd
    , o "conf"           LikeSize          relopConf
    , o "enclose"        LikeSize          relopEnclose
    , o "hang"           LikeMeet          relopHang
    , o "hold"           LikeHold          relopHold
    , o "maybe"          LikeMeet          relopMaybe
    , o "maybe-both"     LikeMeet          relopMaybeBoth
    , o "prefix"         LikePrefix        relopPrefix
    , o "prefix-change"  LikePrefixChange  relopPrefixChange
    , o "range"          LikeSize          relopRange
    , o "rank"           LikeId            relopRank
    , o "rdf"            LikeSource        relopRdf
    , o "size"           LikeSize          relopSize
    , o "typename"       LikeHold          relopTypename
    , o "unprefix"       LikeUnprefix      relopUnprefix
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


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Implement
( vanillaRops

  -- * Operators
  -- $Operators
) where

import Data.Monoid
import Koshucode.Baala.Core
import qualified Koshucode.Baala.Builtin as Kit
import qualified Koshucode.Baala.Minimal as Mini

import Koshucode.Baala.Vanilla.Relmap.Calc
import Koshucode.Baala.Vanilla.Relmap.Naming
import Koshucode.Baala.Vanilla.Relmap.Operand
import Koshucode.Baala.Vanilla.Relmap.Binary
import Koshucode.Baala.Vanilla.Relmap.Unary
import Koshucode.Baala.Vanilla.Type.Relval



-- ----------------------  Operators

{-| Implementation of relational operators. -}
vanillaRops :: [Rop VContent]
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



-- ----------------------  Constructors

relopRdf :: RopCons VContent
relopRdf use = do
  sign  <- Kit.getWord  use "-sign"
  [s,o] <- Kit.getTerms use "-term"
  Right $ relmapAlias use $
        relmapSource use sign ["/s", "/o"] `mappend`
        Mini.relmapRename use [(s,"/s"), (o,"/o")]



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



{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Implement
( vanillaOperators

-- * Operators
-- $Operators
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Vanilla.Relmap.Calc
import Koshucode.Baala.Vanilla.Relmap.Naming
import Koshucode.Baala.Vanilla.Relmap.Operand
import Koshucode.Baala.Vanilla.Relmap.Binary
import Koshucode.Baala.Vanilla.Relmap.Unary
import Koshucode.Baala.Vanilla.Value.Relval
import qualified Koshucode.Baala.Minimal as Mini



-- ----------------------  Operators

{-| Implementation of relational operators. -}
vanillaOperators :: [Kit.OpImplement Val]
vanillaOperators = vanillaOperators' ++ Mini.minimalOperators

vanillaOperators' :: [Kit.OpImplement Val]
vanillaOperators' = Mini.operators
    -- Relmap operators in alphabetical order
    [ o "conf"           LikeSize          relopConf
    , o "enclose"        LikeSize          relopEnclose
    , o "hang"           LikeMeet          relopHang
    , o "hold"           LikeHold          relopHold
    , o "maybe"          LikeMeet          relopMaybe
    , o "maybe-both"     LikeMeet          relopMaybeBoth
    , o "prefix"         LikePrefix        relopPrefix
    , o "prefix-change"  LikePrefixChange  relopPrefixChange
    , o "range"          LikeSize          relopRange
    , o "rdf"            LikeSource        relopRdf
    , o "size"           LikeSize          relopSize
    , o "unhold"         LikeHold          relopUnhold
    , o "unprefix"       LikeUnprefix      relopUnprefix
    , o "val"            LikeVal           relopVal
    ] where o = (,,)



-- ----------------------  Constructors

relopRdf :: Kit.Relop Val
relopRdf use = do
  sign  <- Mini.getWord  use "-sign"
  [s,o] <- Mini.getTerms use "-term"
  Right $ Kit.relmapAlias use $
        Kit.relmapSource use sign ["/s", "/o"] `mappend`
        Mini.relmapRename use [(s,"/s"), (o,"/o")]



-- ----------------------
-- $Operators
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
-- [@val@]


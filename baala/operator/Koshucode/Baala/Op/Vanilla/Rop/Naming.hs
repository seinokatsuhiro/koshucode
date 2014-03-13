{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Vanilla.Rop.Naming
( -- * prefix
  consPrefix, relmapPrefix, relkitPrefix,
  -- * unprefix
  consUnprefix, relmapUnprefix, relkitUnprefix,
  -- * prefix-change
  consPrefixChange, relmapPrefixChange, relkitPrefixChange,
) where

import qualified Data.List as List
import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op



-- ----------------------  prefix

consPrefix :: C.RopCons c
consPrefix use =
    do pre <- Op.getTerm  use "-prefix"
       ns  <- Op.getTerms use "-term"
       Right $ relmapPrefix use pre ns

relmapPrefix :: C.RopUse c -> String -> [String] -> C.Relmap c
relmapPrefix use pre ns = C.relmapFlow use $ relkitPrefix pre ns

-- | Add prefix to specified terms.
relkitPrefix :: String -> [String] -> C.RelkitCalc c
relkitPrefix _ _ Nothing = Right C.relkitNothing
relkitPrefix pre ns (Just he1) = Right kit2 where
    he2 =  B.headChange (map f) he1
    kit2 = C.relkitId $ Just he2
    f n | n `elem` ns  = prefixName pre n
        | otherwise    = n

prefixName :: String -> String -> String
prefixName pre ('/' : ns) = pre ++ "-" ++ ns
prefixName _ _ = undefined



-- ----------------------  unprefix

consUnprefix :: C.RopCons c
consUnprefix use =
    do pre <- Op.getTerm use "-prefix"
       Right $ relmapUnprefix use pre

relmapUnprefix :: C.RopUse c -> String -> C.Relmap c
relmapUnprefix use = C.relmapFlow use . relkitUnprefix

-- | Remove prefix
relkitUnprefix :: String -> C.RelkitCalc c
relkitUnprefix _ Nothing = Right C.relkitNothing
relkitUnprefix pre (Just he1) = Right kit2 where
    he2  = B.headChange (map $ unprefixName pre) he1
    kit2 = C.relkitId $ Just he2

unprefixName :: String -> String -> String
unprefixName pre n =
    case List.stripPrefix pre n of
      Just ('-' : n2) -> '/' : n2
      _ -> n



-- ----------------------  prefix-change

consPrefixChange :: C.RopCons c
consPrefixChange use =
    do new <- Op.getTerm use "-new"
       old <- Op.getTerm use "-old"
       Right $ relmapPrefixChange use (new, old)

relmapPrefixChange :: C.RopUse c -> (String, String) -> C.Relmap c
relmapPrefixChange use = C.relmapFlow use . relkitPrefixChange

-- | Change prefix
relkitPrefixChange :: (String, String) -> C.RelkitCalc c
relkitPrefixChange _ Nothing = Right C.relkitNothing
relkitPrefixChange (new, old) (Just he1) = Right kit2 where
    he2  = B.headChange (map f) he1
    kit2 = C.relkitId $ Just he2
    new' = new ++ "-"
    old' = old ++ "-"
    f n' = case List.stripPrefix old' n' of
             Just n2 -> new' ++ n2
             Nothing -> n'


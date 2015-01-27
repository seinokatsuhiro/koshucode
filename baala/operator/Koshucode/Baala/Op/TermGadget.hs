{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.TermGadget
  ( ropsTermGadget,
    -- * prefix
    consPrefix, relmapPrefix, relkitPrefix,
    -- * unprefix
    consUnprefix, relmapUnprefix, relkitUnprefix,
    -- * prefix-change
    consPrefixChange, relmapPrefixChange, relkitPrefixChange,
    -- * wipe
    consWipe, relmapWipe, relkitWipe,
  ) where

import qualified Data.List                  as List
import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Term    as Op


-- | Relmap operators for manipulating term names.
--
--   [@prefix \/P \/N ...@]
--     Add prefix @\/P@ to terms @\/N@ ...
-- 
--   [@prefix-change \/P \/Q@]
--     Change prefix from @\/P@ to @\/Q@.
-- 
--   [@unprefix \/P@]
--     Remove prefix @\/P@ from term name.
-- 
--   [@wipe@]
--     Cut working terms.
-- 
ropsTermGadget :: (Ord c) => [C.Rop c]
ropsTermGadget = Op.ropList "term"  -- GROUP
    --        CONSTRUCTOR        USAGE                      ATTRIBUTE
    [ Op.def  consPrefix         "prefix /P /N ..."         "1V -prefix -term"
    , Op.def  consPrefixChange   "prefix-change /P /Q"      "2 -new -old"
    , Op.def  consUnprefix       "unprefix /P"              "1 -prefix"
    , Op.def  consWipe           "wipe"                     "0"
    ]


-- ----------------------  prefix

consPrefix :: C.RopCons c
consPrefix use =
    do pre <- Op.getTerm  use "-prefix"
       ns  <- Op.getTerms use "-term"
       Right $ relmapPrefix use pre ns

relmapPrefix :: C.RopUse c -> String -> [String] -> C.Relmap c
relmapPrefix use pre ns = C.relmapFlow use $ relkitPrefix pre ns

-- | Add prefix to specified terms.
relkitPrefix :: String -> [String] -> C.RelkitFlow c
relkitPrefix _ _ Nothing = Right C.relkitNothing
relkitPrefix pre ns (Just he1) = Right kit2 where
    he2 =  B.headMapName f he1
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
relkitUnprefix :: String -> C.RelkitFlow c
relkitUnprefix _ Nothing = Right C.relkitNothing
relkitUnprefix pre (Just he1) = Right kit2 where
    he2  = B.headMapName (unprefixName pre) he1
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
relkitPrefixChange :: (String, String) -> C.RelkitFlow c
relkitPrefixChange _ Nothing = Right C.relkitNothing
relkitPrefixChange (new, old) (Just he1) = Right kit2 where
    he2  = B.headMapName f he1
    kit2 = C.relkitId $ Just he2
    new' = new ++ "-"
    old' = old ++ "-"
    f n' = case List.stripPrefix old' n' of
             Just n2 -> new' ++ n2
             Nothing -> n'


-- ----------------------  wipe

consWipe :: C.RopCons c
consWipe = Right . relmapWipe

relmapWipe :: C.RopUse c -> C.Relmap c
relmapWipe use = C.relmapFlow use relkitWipe

relkitWipe :: C.RelkitFlow c
relkitWipe Nothing = Right C.relkitNothing
relkitWipe (Just he1) = Op.relkitCut ns1 (Just he1) where
    ns1 = filter (elem '=') $ B.headNames he1

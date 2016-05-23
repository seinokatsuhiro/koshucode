{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Flat.TermGadget
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

import qualified Data.List                   as List
import qualified Koshucode.Baala.Data        as D
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Rop.Base    as Op
import qualified Koshucode.Baala.Rop.Flat.Term    as Op


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
    [ Op.def  consPrefix         "prefix /P /N ..."         "-prefix -term*"
    , Op.def  consPrefixChange   "prefix-change /P /Q"      "-new -old"
    , Op.def  consUnprefix       "unprefix /P"              "-prefix"
    , Op.def  consWipe           "wipe"                     ""
    ]


-- ----------------------  prefix

consPrefix :: C.RopCons c
consPrefix med =
    do pre <- Op.getTerm  med "-prefix"
       ns  <- Op.getTerms med "-term"
       Right $ relmapPrefix med pre ns

relmapPrefix :: C.Intmed c -> String -> [String] -> C.Relmap c
relmapPrefix med pre ns = C.relmapFlow med $ relkitPrefix pre ns

-- | Add prefix to specified terms.
relkitPrefix :: String -> [String] -> C.RelkitFlow c
relkitPrefix _ _ Nothing = Right C.relkitNothing
relkitPrefix pre ns (Just he1) = Right kit2 where
    he2 =  D.headMapName f he1
    kit2 = C.relkitId $ Just he2
    f n | n `elem` ns  = prefixName pre n
        | otherwise    = n

prefixName :: String -> String -> String
prefixName pre ns = pre ++ "-" ++ ns


-- ----------------------  unprefix

consUnprefix :: C.RopCons c
consUnprefix med =
    do pre <- Op.getTerm med "-prefix"
       Right $ relmapUnprefix med pre

relmapUnprefix :: C.Intmed c -> String -> C.Relmap c
relmapUnprefix med = C.relmapFlow med . relkitUnprefix

-- | Remove prefix
relkitUnprefix :: String -> C.RelkitFlow c
relkitUnprefix _ Nothing = Right C.relkitNothing
relkitUnprefix pre (Just he1) = Right kit2 where
    he2  = D.headMapName (unprefixName pre) he1
    kit2 = C.relkitId $ Just he2

unprefixName :: String -> String -> String
unprefixName pre n =
    case List.stripPrefix pre n of
      Just ('-' : n2) -> n2
      _ -> n


-- ----------------------  prefix-change

consPrefixChange :: C.RopCons c
consPrefixChange med =
    do new <- Op.getTerm med "-new"
       old <- Op.getTerm med "-old"
       Right $ relmapPrefixChange med (new, old)

relmapPrefixChange :: C.Intmed c -> (String, String) -> C.Relmap c
relmapPrefixChange med = C.relmapFlow med . relkitPrefixChange

-- | Change prefix
relkitPrefixChange :: (String, String) -> C.RelkitFlow c
relkitPrefixChange _ Nothing = Right C.relkitNothing
relkitPrefixChange (new, old) (Just he1) = Right kit2 where
    he2  = D.headMapName f he1
    kit2 = C.relkitId $ Just he2
    new' = new ++ "-"
    old' = old ++ "-"
    f n' = case List.stripPrefix old' n' of
             Just n2 -> new' ++ n2
             Nothing -> n'


-- ----------------------  wipe

consWipe :: C.RopCons c
consWipe = Right . relmapWipe

relmapWipe :: C.Intmed c -> C.Relmap c
relmapWipe med = C.relmapFlow med relkitWipe

relkitWipe :: C.RelkitFlow c
relkitWipe Nothing = Right C.relkitNothing
relkitWipe (Just he1) = Op.relkitCut ns1 (Just he1) where
    ns1 = filter (elem '=') $ D.headNames he1

{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Resource
  ( ropsResource,
    -- * koshu-res-rop
    consKoshuResRop, relkitKoshuResRop,
    -- * koshu-res-sink
    consKoshuResSink, relkitKoshuResSink,
    -- * koshu-res-article
    consKoshuResArticle, relkitKoshuResArticle,
  ) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Data          as C
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Op.Builtin    as Op


-- | Relmap operators about resources.
--
--   [@koshu-res-rop \/N \/N@]
--     Derived relmap operators in the current resource.
-- 
--   [@koshu-res-sink \/N \/N@]
--     Judgement patterns of sinks in the current resource.
-- 
ropsResource :: (C.CContent c) => [C.Rop c]
ropsResource = Op.ropList "resource"
    --        CONSTRUCTOR          USAGE                   ATTRIBUTE
    [ Op.def  consKoshuResArticle  "koshu-res-article /N"  "1 -name"
    , Op.def  consKoshuResRop      "koshu-res-rop /N /N"   "2 -sec -name"
    , Op.def  consKoshuResSink     "koshu-res-sink /N /N"  "2 -sec -pat"
    , Op.def  Op.consXxx           "koshu-res-source /N"   "1 -pat"
    , Op.def  Op.consXxx           "koshu-res-sink-source /N /N" "2 -sink -source"
    ]


-- ----------------------  koshu-res-rop

consKoshuResRop :: (C.CContent c) => C.RopCons c
consKoshuResRop med =
  do sec   <- Op.getTerm med "-sec"
     name  <- Op.getTerm med "-name"
     Right $ relmapKoshuResRop med (sec, name)

relmapKoshuResRop :: (C.CContent c)
    => C.Intmed c -> (B.TermName, B.TermName)
    -> C.Relmap c
relmapKoshuResRop med = C.relmapHook med . relkitKoshuResRop

relkitKoshuResRop :: (C.CContent c)
    => (B.TermName, B.TermName)
    -> C.RelkitHook c
relkitKoshuResRop (sec, name) res _ = Right kit2 where
    kit2  = C.relkitConstBody ns bo2
    ns    = [sec, name]
    bo2   = f `map` C.resLexmap res
    f ((s, n), _) = [C.pInt s, C.pText n]


-- ----------------------  koshu-res-sink

consKoshuResSink :: (C.CContent c) => C.RopCons c
consKoshuResSink med =
  do sec   <- Op.getTerm med "-sec"
     pat   <- Op.getTerm med "-pat"
     Right $ relmapKoshuResSink med (sec, pat)

relmapKoshuResSink :: (C.CContent c)
    => C.Intmed c -> (B.TermName, B.TermName)
    -> C.Relmap c
relmapKoshuResSink med = C.relmapHook med . relkitKoshuResSink

relkitKoshuResSink :: (C.CContent c)
    => (B.TermName, B.TermName)
    -> C.RelkitHook c
relkitKoshuResSink (sec, pat) res _ = Right kit2 where
    kit2  = C.relkitConstBody ns bo2
    ns    = [sec, pat]
    g a   = [C.pInt $ C.assSection a, C.pText $ C.assPattern a]
    f     = g . B.shortBody
    bo2   = f `map` C.resAssert res


-- ----------------------  koshu-res-article

consKoshuResArticle :: (C.CContent c) => C.RopCons c
consKoshuResArticle med =
  do name <- Op.getTerm med "-name"
     Right $ relmapKoshuResArticle med name

relmapKoshuResArticle :: (C.CContent c) => C.Intmed c -> B.TermName -> C.Relmap c
relmapKoshuResArticle med = C.relmapHook med . relkitKoshuResArticle

relkitKoshuResArticle :: (C.CContent c) => B.TermName -> C.RelkitHook c
relkitKoshuResArticle name res _ = Right kit2 where
    kit2  = C.relkitConstBody ns bo2
    ns    = [name]
    f s   = [C.pText $ B.ioPointText $ B.codeName s]
    bo2   = f `map` C.resIncluded res


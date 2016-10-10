{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Flat.Resource
  ( ropsResource,
    -- * koshu-res-rop
    consKoshuResRop, relkitKoshuResRop,
    -- * koshu-res-sink
    consKoshuResSink, relkitKoshuResSink,
    -- * koshu-res-article
    consKoshuResArticle, relkitKoshuResArticle,
  ) where

import qualified Koshucode.Baala.Base           as B
import qualified Koshucode.Baala.Syntax         as S
import qualified Koshucode.Baala.Data           as D
import qualified Koshucode.Baala.Core           as C
import qualified Koshucode.Baala.Rop.Base       as Op


-- | Relmap operators about resources.
--
--   [@koshu-res-rop \/N \/N@]
--     Derived relmap operators in the current resource.
-- 
--   [@koshu-res-sink \/N \/N@]
--     Judgement patterns of sinks in the current resource.
-- 
ropsResource :: (D.CContent c) => [C.Rop c]
ropsResource = Op.ropList "resource"
    --        CONSTRUCTOR          USAGE                   ATTRIBUTE
    [ Op.def  consKoshuResArticle  "koshu-res-article /N"  "-name"
    , Op.def  consKoshuResRop      "koshu-res-rop /N /N"   "-sec -name"
    , Op.def  consKoshuResSink     "koshu-res-sink /N /N"  "-sec -pat"
    , Op.def  Op.consXxx           "koshu-res-source /N"   "-pat"
    , Op.def  Op.consXxx           "koshu-res-sink-source /N /N" "-sink -source"
    ]


-- ----------------------  koshu-res-rop

consKoshuResRop :: (D.CContent c) => C.RopCons c
consKoshuResRop med =
  do sec   <- Op.getTerm med "-sec"
     name  <- Op.getTerm med "-name"
     Right $ relmapKoshuResRop med (sec, name)

relmapKoshuResRop :: (D.CContent c)
    => C.Intmed c -> (S.TermName, S.TermName)
    -> C.Relmap c
relmapKoshuResRop med = C.relmapHook med . relkitKoshuResRop

-- | Create @koshu-res-rop@ relkit.
relkitKoshuResRop :: (D.CContent c)
    => (S.TermName, S.TermName)
    -> C.RelkitHook c
relkitKoshuResRop (sec, name) res _ = Right kit2 where
    kit2  = C.relkitConstBody ns bo2
    ns    = [sec, name]
    bo2   = f `map` C.resLexmap res
    f ((s, n), _) = [D.pInt s, D.pText n]


-- ----------------------  koshu-res-sink

consKoshuResSink :: (D.CContent c) => C.RopCons c
consKoshuResSink med =
  do sec   <- Op.getTerm med "-sec"
     pat   <- Op.getTerm med "-pat"
     Right $ relmapKoshuResSink med (sec, pat)

relmapKoshuResSink :: (D.CContent c)
    => C.Intmed c -> (S.TermName, S.TermName)
    -> C.Relmap c
relmapKoshuResSink med = C.relmapHook med . relkitKoshuResSink

-- | Create @koshu-res-sink@ relkit.
relkitKoshuResSink :: (D.CContent c)
    => (S.TermName, S.TermName)
    -> C.RelkitHook c
relkitKoshuResSink (sec, pat) res _ = Right kit2 where
    kit2  = C.relkitConstBody ns bo2
    ns    = [sec, pat]
    g a   = [D.pInt $ C.assSection a, D.pText $ C.assClass a]
    f     = g . S.shortBody
    bo2   = f `map` C.resAssert res


-- ----------------------  koshu-res-article

consKoshuResArticle :: (D.CContent c) => C.RopCons c
consKoshuResArticle med =
  do name <- Op.getTerm med "-name"
     Right $ relmapKoshuResArticle med name

relmapKoshuResArticle :: (D.CContent c) => C.Intmed c -> S.TermName -> C.Relmap c
relmapKoshuResArticle med = C.relmapHook med . relkitKoshuResArticle

-- | Create @koshu-res-article@ relkit.
relkitKoshuResArticle :: (D.CContent c) => S.TermName -> C.RelkitHook c
relkitKoshuResArticle name res _ = Right kit2 where
    kit2  = C.relkitConstBody ns bo2
    ns    = [name]
    f s   = [D.pText $ B.ioPointText $ B.nioPoint s]
    bo2   = f `map` C.resIncluded res


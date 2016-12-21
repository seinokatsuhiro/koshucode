{-# OPTIONS_GHC -Wall #-}

-- | Operators on data resources.

module Koshucode.Baala.Rop.Flat.Resource
  ( ropsResource,
    -- * koshu-res-rop
    consKoshuResRop, relkitKoshuResRop,
    -- * koshu-res-sink
    consKoshuResSink, relkitKoshuResSink,
    -- * koshu-res-article
    consKoshuResArticle, relkitKoshuResArticle,
  ) where

import qualified Koshucode.Baala.DataPlus       as K
import qualified Koshucode.Baala.Core           as C
import qualified Koshucode.Baala.Rop.Base       as Rop


-- | Relmap operators about resources.
ropsResource :: (K.CContent c) => [C.Rop c]
ropsResource = Rop.rops "resource"
    [ consKoshuResArticle K.& [ "koshu-res-article /N"  K.& "-name" ]
    , consKoshuResRop     K.& [ "koshu-res-rop /N /N"   K.& "-sec -name" ]
    , consKoshuResSink    K.& [ "koshu-res-sink /N /N"  K.& "-sec -pat" ]
    , Rop.consXxx         K.& [ "koshu-res-source /N"   K.& "-pat" ]
    , Rop.consXxx         K.& [ "koshu-res-sink-source /N /N"
                                K.& "-sink -source" ]
    ]


-- ----------------------  koshu-res-rop

-- | __koshu-res-rop \/N \/N__
--
--   Derived relmap operators in the current resource.
--
consKoshuResRop :: (K.CContent c) => C.RopCons c
consKoshuResRop med =
  do sec   <- Rop.getTerm med "-sec"
     name  <- Rop.getTerm med "-name"
     Right $ relmapKoshuResRop med (sec, name)

-- | Create @koshu-res-rop@ relmap.
relmapKoshuResRop :: (K.CContent c)
    => C.Intmed c -> (K.TermName, K.TermName)
    -> C.Relmap c
relmapKoshuResRop med = C.relmapHook med . relkitKoshuResRop

-- | Create @koshu-res-rop@ relkit.
relkitKoshuResRop :: (K.CContent c)
    => (K.TermName, K.TermName)
    -> C.RelkitHook c
relkitKoshuResRop (sec, name) res _ = Right kit2 where
    kit2  = C.relkitConstBody ns bo2
    ns    = [sec, name]
    bo2   = f `map` C.resLexmap res
    f ((s, n), _) = [K.pInt s, K.pText n]


-- ----------------------  koshu-res-sink

-- | __koshu-res-sink \/N \/N__
--
--   Judgement class of sinks in the current resource.
--
consKoshuResSink :: (K.CContent c) => C.RopCons c
consKoshuResSink med =
  do sec   <- Rop.getTerm med "-sec"
     pat   <- Rop.getTerm med "-pat"
     Right $ relmapKoshuResSink med (sec, pat)

-- | Create @koshu-res-sink@ relmap.
relmapKoshuResSink :: (K.CContent c)
    => C.Intmed c -> (K.TermName, K.TermName)
    -> C.Relmap c
relmapKoshuResSink med = C.relmapHook med . relkitKoshuResSink

-- | Create @koshu-res-sink@ relkit.
relkitKoshuResSink :: (K.CContent c)
    => (K.TermName, K.TermName)
    -> C.RelkitHook c
relkitKoshuResSink (sec, pat) res _ = Right kit2 where
    kit2  = C.relkitConstBody ns bo2
    ns    = [sec, pat]
    g a   = [K.pInt $ C.assSection a, K.pText $ C.assClass a]
    f     = g . K.shortBody
    bo2   = f `map` C.resAssert res


-- ----------------------  koshu-res-article

-- | __koshu-res-article \/N__
consKoshuResArticle :: (K.CContent c) => C.RopCons c
consKoshuResArticle med =
  do name <- Rop.getTerm med "-name"
     Right $ relmapKoshuResArticle med name

-- | Create @koshu-res-article@ relmap.
relmapKoshuResArticle :: (K.CContent c) => C.Intmed c -> K.TermName -> C.Relmap c
relmapKoshuResArticle med = C.relmapHook med . relkitKoshuResArticle

-- | Create @koshu-res-article@ relkit.
relkitKoshuResArticle :: (K.CContent c) => K.TermName -> C.RelkitHook c
relkitKoshuResArticle name res _ = Right kit2 where
    kit2  = C.relkitConstBody ns bo2
    ns    = [name]
    f s   = [K.pText $ K.ioPointText $ K.nioPoint s]
    bo2   = f `map` C.resIncluded res


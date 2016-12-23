{-# OPTIONS_GHC -Wall #-}

-- | Operation for term names.

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

import qualified Data.List                      as List
import qualified Koshucode.Baala.DataPlus       as K
import qualified Koshucode.Baala.Core           as C
import qualified Koshucode.Baala.Rop.Base       as Rop
import qualified Koshucode.Baala.Rop.Flat.Term  as Rop


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
ropsTermGadget = Rop.rops "term"
    [ consPrefix        K.& [ "prefix /N -to /P ..."   K.& "to : -prefix . -to"
                            , "prefix /N /P ..."       K.& "pos : -prefix -term*" ]
    , consPrefixChange  K.& [ "prefix-change /P /Q"    K.& "-new -old" ]
    , consUnprefix      K.& [ "unprefix /P"            K.& "-prefix" ]
    , consWipe          K.& [ "wipe"                   K.& "" ]
    ]


-- ----------------------  prefix

-- | Add prefix to specified terms.
consPrefix :: C.RopCons c
consPrefix med =
    do let tag = Rop.getTag med "to"
       pre <- Rop.getTerm  med "-prefix"
       to  <- Rop.getTerms med $ if tag then "-to" else "-term"
       Right $ relmapPrefix med pre to

-- | Create @prefix@ relmap.
relmapPrefix :: C.Intmed c -> K.TermName -> [K.TermName] -> C.Relmap c
relmapPrefix med pre ns = C.relmapFlow med $ relkitPrefix pre ns

-- | Create @prefix@ relkit.
relkitPrefix :: K.TermName -> [K.TermName] -> C.RelkitFlow c
relkitPrefix _ _ Nothing = C.relkitUnfixed
relkitPrefix pre ns (Just he1) = Right kit2 where
    he2 =  K.headMapName f he1
    kit2 = C.relkitId $ Just he2
    f n | n `elem` ns  = prefixName pre n
        | otherwise    = n

prefixName :: K.TermName -> K.TermName -> K.TermName
prefixName pre n = K.toTermName
    $ K.termNameContent pre ++ "-" ++ K.termNameContent n

    
-- ----------------------  unprefix

-- | Remove prefix.
consUnprefix :: C.RopCons c
consUnprefix med =
    do pre <- Rop.getTerm med "-prefix"
       Right $ relmapUnprefix med pre

-- | Create @unprefix@ relmap.
relmapUnprefix :: C.Intmed c -> K.TermName -> C.Relmap c
relmapUnprefix med = C.relmapFlow med . relkitUnprefix

-- | Create @unprefix@ relkit.
relkitUnprefix :: K.TermName -> C.RelkitFlow c
relkitUnprefix _ Nothing = C.relkitUnfixed
relkitUnprefix pre (Just he1) = Right kit2 where
    he2  = K.headMapName (unprefixName pre) he1
    kit2 = C.relkitId $ Just he2

unprefixName :: K.TermName -> K.TermName -> K.TermName
unprefixName pre n =
    case List.stripPrefix (K.termNameContent pre) (K.termNameContent n) of
      Just ('-' : n2) -> K.toTermName n2
      _ -> n


-- ----------------------  prefix-change

-- | Change prefix.
consPrefixChange :: C.RopCons c
consPrefixChange med =
    do new <- Rop.getTerm med "-new"
       old <- Rop.getTerm med "-old"
       Right $ relmapPrefixChange med (new, old)

-- | Create @prefix-change@ relmap.
relmapPrefixChange :: C.Intmed c -> K.TermName2 -> C.Relmap c
relmapPrefixChange med = C.relmapFlow med . relkitPrefixChange

-- | Create @prefix-change@ relkit.
relkitPrefixChange :: K.TermName2 -> C.RelkitFlow c
relkitPrefixChange _ Nothing = C.relkitUnfixed
relkitPrefixChange (new, old) (Just he1) = Right kit2 where
    he2  = K.headMapName f he1
    kit2 = C.relkitId $ Just he2
    new' = K.termNameContent new ++ "-"
    old' = K.termNameContent old ++ "-"
    f n' = case List.stripPrefix old' (K.termNameContent n') of
             Just n2 -> K.toTermName $ new' ++ n2
             Nothing -> n'


-- ----------------------  wipe

-- | __wipe__
consWipe :: C.RopCons c
consWipe = Right . relmapWipe

-- | Create @wipe@ relmap.
relmapWipe :: C.Intmed c -> C.Relmap c
relmapWipe med = C.relmapFlow med relkitWipe

-- | Create @wipe@ relkit.
relkitWipe :: C.RelkitFlow c
relkitWipe Nothing = C.relkitUnfixed
relkitWipe (Just he1) = Rop.relkitCut ns1 (Just he1) where
    ns1 = (\n -> elem '=' $ K.termNameContent n) `filter` K.getTermNames he1

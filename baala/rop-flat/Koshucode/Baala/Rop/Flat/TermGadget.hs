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
import qualified Koshucode.Baala.Overture       as O
import qualified Koshucode.Baala.Syntax         as S
import qualified Koshucode.Baala.Data           as D
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
ropsTermGadget = Rop.ropList "term"  -- GROUP
    --        CONSTRUCTOR          USAGE                        ATTRIBUTE
    [ Rop.rop consPrefix         [ "prefix /N -to /P ..."   O.& "pos : -prefix -term* | to : -prefix . -to" ]
    , Rop.rop consPrefixChange   [ "prefix-change /P /Q"    O.& "-new -old" ]
    , Rop.rop consUnprefix       [ "unprefix /P"            O.& "-prefix" ]
    , Rop.rop consWipe           [ "wipe"                   O.& "" ]
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
relmapPrefix :: C.Intmed c -> S.TermName -> [S.TermName] -> C.Relmap c
relmapPrefix med pre ns = C.relmapFlow med $ relkitPrefix pre ns

-- | Create @prefix@ relkit.
relkitPrefix :: S.TermName -> [S.TermName] -> C.RelkitFlow c
relkitPrefix _ _ Nothing = Right C.relkitNothing
relkitPrefix pre ns (Just he1) = Right kit2 where
    he2 =  D.headMapName f he1
    kit2 = C.relkitId $ Just he2
    f n | n `elem` ns  = prefixName pre n
        | otherwise    = n

prefixName :: S.TermName -> S.TermName -> S.TermName
prefixName pre n = S.toTermName
    $ S.termNameContent pre ++ "-" ++ S.termNameContent n

    
-- ----------------------  unprefix

-- | Remove prefix.
consUnprefix :: C.RopCons c
consUnprefix med =
    do pre <- Rop.getTerm med "-prefix"
       Right $ relmapUnprefix med pre

-- | Create @unprefix@ relmap.
relmapUnprefix :: C.Intmed c -> S.TermName -> C.Relmap c
relmapUnprefix med = C.relmapFlow med . relkitUnprefix

-- | Create @unprefix@ relkit.
relkitUnprefix :: S.TermName -> C.RelkitFlow c
relkitUnprefix _ Nothing = Right C.relkitNothing
relkitUnprefix pre (Just he1) = Right kit2 where
    he2  = D.headMapName (unprefixName pre) he1
    kit2 = C.relkitId $ Just he2

unprefixName :: S.TermName -> S.TermName -> S.TermName
unprefixName pre n =
    case List.stripPrefix (S.termNameContent pre) (S.termNameContent n) of
      Just ('-' : n2) -> S.toTermName n2
      _ -> n


-- ----------------------  prefix-change

-- | Change prefix.
consPrefixChange :: C.RopCons c
consPrefixChange med =
    do new <- Rop.getTerm med "-new"
       old <- Rop.getTerm med "-old"
       Right $ relmapPrefixChange med (new, old)

-- | Create @prefix-change@ relmap.
relmapPrefixChange :: C.Intmed c -> S.TermName2 -> C.Relmap c
relmapPrefixChange med = C.relmapFlow med . relkitPrefixChange

-- | Create @prefix-change@ relkit.
relkitPrefixChange :: S.TermName2 -> C.RelkitFlow c
relkitPrefixChange _ Nothing = Right C.relkitNothing
relkitPrefixChange (new, old) (Just he1) = Right kit2 where
    he2  = D.headMapName f he1
    kit2 = C.relkitId $ Just he2
    new' = S.termNameContent new ++ "-"
    old' = S.termNameContent old ++ "-"
    f n' = case List.stripPrefix old' (S.termNameContent n') of
             Just n2 -> S.toTermName $ new' ++ n2
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
relkitWipe Nothing = Right C.relkitNothing
relkitWipe (Just he1) = Rop.relkitCut ns1 (Just he1) where
    ns1 = (\n -> elem '=' $ S.termNameContent n) `filter` D.getTermNames he1

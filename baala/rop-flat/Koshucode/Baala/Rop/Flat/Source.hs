{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Flat.Source
  ( ropsSource,
  
    -- * dee & dum
    consDee, consDum,
    -- $deedum
  
    -- * empty
    consEmpty, relmapEmpty,
  
    -- * source
    consSource,
    -- $source
  
    -- * source-term
    consSourceTerm, relmapSourceTerm,
    -- $source-term
  ) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Syntax       as S
import qualified Koshucode.Baala.Data         as D
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Rop.Base     as Op


-- | Implementation of relational operators.
--
--   [@dee@]        Nullary full relation.
--
--   [@dum@]        Nullary empty relation.
--
--   [@empty@]      Make empty relation.
--
--   [@source@]     Read relation from data source.
--
ropsSource :: (D.CContent c) => [C.Rop c]
ropsSource = Op.ropList "source"
    --        CONSTRUCTOR     USAGE               ATTRIBUTE
    [ Op.def  consDee         "dee"               ""
    , Op.def  consDum         "dum"               ""
    , Op.def  consEmpty       "empty /N ..."      "-term*"
    , Op.def  consSource      "source P /N ..."   "-pattern -term*"
    , Op.def  consSourceTerm  "source-term P R"   "-pattern -relmap/"
    ]


-- ----------------------  empty

consEmpty :: C.RopCons c
consEmpty med =
    do ns <- Op.getTerms med "-term"
       Right $ relmapEmpty med ns

relmapEmpty :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapEmpty med = C.relmapFlow med . relkitEmpty

relkitEmpty :: [S.TermName] -> C.RelkitFlow c
relkitEmpty ns _ = Right $ C.relkit he2 $ C.RelkitConst [] where
    he2 = Just $ D.headFrom ns


-- ----------------------  source

-- $source
-- 
--  Read relation with term @\/a@ and @\/b@ constructed from judges of @P@.
-- 
--    > source P /a /b

consSource :: C.RopCons c
consSource med =
  do pattern  <- Op.getWord  med "-pattern"
     terms    <- Op.getTerms med "-term"
     Right $ C.relmapSource med pattern terms


-- ----------------------  source-term

-- $source-term
-- 
--  Define relmap @p2@ that has same terms as @p1@.
-- 
--    > p1 : source P1 /a /b
--    > p2 : source-term P2 p2

consSourceTerm :: C.RopCons c
consSourceTerm med =
  do pat   <- Op.getWord   med "-pattern"
     rmap  <- Op.getRelmap med "-relmap"
     Right $ relmapSourceTerm med pat rmap

relmapSourceTerm :: C.Intmed c -> String -> B.Map (C.Relmap c)
relmapSourceTerm med pat = C.relmapBinary med $ relkitSourceTerm pat

relkitSourceTerm :: String -> C.RelkitBinary c
relkitSourceTerm _   (C.Relkit _ Nothing    _) _ = Right C.relkitNothing
relkitSourceTerm pat (C.Relkit _ (Just he2) _) _ = Right kit3 where
    kit3   = C.relkitJust he2 $ C.RelkitSource pat ns2
    ns2    = D.headNames he2


-- ----------------------  dee & dum

-- $deedum
--
--  Nullary fullset relation.
--
--    > dee
--
--  Nullary empty relation.
--
--    > dum

consDee :: C.RopCons c
consDee med = Right $ C.relmapConst med D.reldee

consDum :: C.RopCons c
consDum med = Right $ C.relmapConst med D.reldum


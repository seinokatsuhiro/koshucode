{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Minimal.Origin
( -- * source
  consSource,
  -- $source

  -- * source-term
  consSourceTerm, relmapSourceTerm,
  -- $source-term

  -- * id
  consId, relmapId,
  -- $id

  -- * empty
  consEmpty, relmapEmpty,

  -- * contents
  consContents, relmapContents,

  -- * dee & dum
  consDee, consDum,
  -- $deedum
) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Op.Builtin as Op


-- ----------------------  source

-- $source
-- 
--  Read relation with term @\/a@ and @\/b@ constructed from judges of @P@.
-- 
--    > source P /a /b

consSource :: C.RopCons c
consSource use =
  do pattern  <- Op.getWord  use "-pattern"
     terms    <- Op.getTerms use "-term"
     Right $ C.relmapSource use pattern terms


-- ----------------------  source-term

-- $source-term
-- 
--  Define relmap @p2@ that has same terms as @p1@.
-- 
--    > p1 : source P1 /a /b
--    > p2 : source-term P2 p2

consSourceTerm :: C.RopCons c
consSourceTerm use =
  do pat   <- Op.getWord  use "-pattern"
     rmap  <- Op.getRelmap use
     Right $ relmapSourceTerm use pat rmap

relmapSourceTerm :: C.RopUse c -> String -> B.Map (C.Relmap c)
relmapSourceTerm use pat = C.relmapBinary use $ relkitSourceTerm pat

relkitSourceTerm :: String -> C.RelkitBinary c
relkitSourceTerm _   (C.Relkit Nothing    _) _ = Right C.relkitNothing
relkitSourceTerm pat (C.Relkit (Just he2) _) _ = Right kit3 where
    kit3   = C.relkitJust he2 $ C.RelkitSource pat ns2
    ns2    = B.headNames he2



-- ----------------------  id

-- $id
--
--  Identity mapping, i.e., do nothing.
--
--    > pick /a /b | id

consId :: C.RopCons c
consId use = Right $ relmapId use

relmapId :: C.RopUse c -> C.Relmap c
relmapId use = C.relmapFlow use $ Right . C.relkitId


-- ----------------------  empty

consEmpty :: C.RopCons c
consEmpty use = Right $ relmapEmpty use

relmapEmpty :: C.RopUse c -> C.Relmap c
relmapEmpty use = C.relmapFlow use relkitEmpty

-- | Throw away all tuples in a relation.
relkitEmpty :: C.RelkitFlow c
relkitEmpty he1 = Right $ C.relkit he1 $ C.RelkitConst []


-- ----------------------  contents

consContents :: (Ord c) => C.RopCons c
consContents use =
    do n <- Op.getTerm use "-term"
       Right $ relmapContents use n

relmapContents :: (Ord c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapContents use = C.relmapFlow use . relkitContents

relkitContents :: (Ord c) => B.TermName -> C.RelkitFlow c
relkitContents n _ = Right $ C.relkitJust he2 $ C.RelkitFull False kitf where
    he2  = B.headFrom [n]
    kitf = map B.singleton . B.unique . concat


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
consDee use = Right $ C.relmapConst use B.reldee

consDum :: C.RopCons c
consDum use = Right $ C.relmapConst use B.reldum


{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Nest.Flow
( -- * down
  consDown, relmapDown, relkitDown,
  -- $DownExample
  -- * up
  consUp, relmapUp, relkitUp,
  -- $UpExample
  -- * join-up
  consJoinUp, relmapJoinUp,
  -- * chunk
  consChunk, relmapChunk, relkitChunk,
) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Op.Builtin    as Op
import qualified Koshucode.Baala.Op.Lattice    as Op
import qualified Koshucode.Baala.Op.Message    as Msg



-- ----------------------  down

-- $DownExample
--
--  Enclose relation from @a@ into term @\/r@.
--  In other words, relation flows down to nested level.
--
--    > a | down /r

consDown :: (C.CRel c) => C.RopCons c
consDown use =
  do n <- Op.getTerm use "-term"
     Right $ relmapDown use n

relmapDown :: (C.CRel c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapDown use = C.relmapFlow use . relkitDown

relkitDown :: (C.CRel c) => B.TermName -> C.RelkitFlow c
relkitDown _ Nothing = Right C.relkitNothing
relkitDown n (Just he1) = Right kit2 where
    he2       = B.Relhead [B.TermNest n $ B.headTerms he1]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = [[ C.pRel $ B.Rel he1 bo1 ]]



-- ----------------------  up

-- $UpExample
--
--  Lift nested relation @\/r@ up onto current flow.
--
--    > b | up /r

consUp :: (C.CRel c) => C.RopCons c
consUp use =
  do n <- Op.getTerm use "-term"
     Right $ relmapUp use n

relmapUp :: (C.CRel c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapUp use = C.relmapFlow use . relkitUp

relkitUp :: (C.CRel c) => B.TermName -> C.RelkitFlow c
relkitUp _ Nothing = Right C.relkitNothing
relkitUp n (Just he1)
    | null ind1       = Msg.unkTerm [n] he1
    | B.isTermNest t1 = Right kit2
    | otherwise       = Msg.notNestRel [n] he1
    where
      ns1   = B.headNames he1
      ind1  = [n] `B.snipIndex` ns1
      pick1 = B.snipFrom ind1
      t1    = head $ pick1 $ B.headTerms he1
      he2   = B.Relhead $ B.termNest t1
      kit2  = C.relkitJust he2 $ C.RelkitOneToMany True kitf2
      kitf2 = B.relBody . C.gRel . head . pick1


-- ----------------------  join-up

consJoinUp :: (Ord c) => C.RopCons c
consJoinUp use =
  do with <- Op.getWithTerms use "-term"
     Right $ relmapJoinUp use with

relmapJoinUp :: (Ord c) => C.RopUse c -> [B.Terminal String] -> C.Relmap c
relmapJoinUp use with = C.relmapWith use with $ Op.relmapJoinList use rmaps where
    rmaps = link `map` map snd with
    link v = C.relmapLink use v []


-- ----------------------  chunk

--  > chunk /a /b /c

consChunk :: (Ord c, C.CRel c) => C.RopCons c
consChunk use =
  do ns  <- Op.getTerms use "-term"
     ord <- Op.getOption [] Op.getTerms use "-order"
     Right $ relmapChunk use ns ord

relmapChunk :: (Ord c, C.CRel c) => C.RopUse c -> [B.TermName] -> [B.TermName] -> C.Relmap c
relmapChunk use ns ord = C.relmapFlow use $ relkitChunk ns ord

relkitChunk :: (Ord c, C.CRel c) => [B.TermName] -> [B.TermName] -> C.RelkitFlow c
relkitChunk _ _ Nothing = Right C.relkitNothing
relkitChunk ns ord (Just he1) = Right kit2 where
    he2     = B.Relhead $ map nest ns
    nest n  = B.TermNest n $ B.headTerms he1
    kit2    = C.relkitJust he2 $ C.RelkitFull False f2
    f2 bo1  = let deg    = length bo1 `B.ceilingRem` length ns
                  bo1'   = B.sortByName (map B.Asc ord) ns bo1
                  ch     = B.chunks deg bo1'
                  rels   = (C.pRel . B.Rel he1) `map` ch
              in [rels]

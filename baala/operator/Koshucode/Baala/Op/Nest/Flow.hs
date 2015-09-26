{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Nest.Flow
  ( -- * down
    consDown, relmapDown, relkitDown,
    -- $DownExample
  
    -- * up
    consUp, relmapUp, relkitUp,
    -- $UpExample
  
    -- * chunk
    consChunk, relmapChunk, relkitChunk,
  ) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Data          as C
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Op.Builtin    as Op
import qualified Koshucode.Baala.Op.Message    as Msg



-- ----------------------  down

-- $DownExample
--
--  Enclose relation from @a@ into term @\/r@.
--  In other words, relation flows down to nested level.
--
--    > a | down /r

consDown :: (C.CRel c) => C.RopCons c
consDown med =
  do n <- Op.getTerm med "-term"
     Right $ relmapDown med n

relmapDown :: (C.CRel c) => C.Intmed c -> B.TermName -> C.Relmap c
relmapDown med = C.relmapFlow med . relkitDown

relkitDown :: (C.CRel c) => B.TermName -> C.RelkitFlow c
relkitDown _ Nothing = Right C.relkitNothing
relkitDown n (Just he1) = Right kit2 where
    he2       = B.headConsNest n he1 $ B.headEmpty
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = [[ C.pRel $ B.Rel he1 bo1 ]]



-- ----------------------  up

-- $UpExample
--
--  Lift nested relation @\/r@ up onto current flow.
--
--    > b | up /r

consUp :: (C.CRel c) => C.RopCons c
consUp med =
  do n <- Op.getTerm med "-term"
     Right $ relmapUp med n

relmapUp :: (C.CRel c) => C.Intmed c -> B.TermName -> C.Relmap c
relmapUp med = C.relmapFlow med . relkitUp

relkitUp :: (C.CRel c) => B.TermName -> C.RelkitFlow c
relkitUp _ Nothing = Right C.relkitNothing
relkitUp n (Just he1)
    | B.headDisjoint lr  = Msg.unkTerm [n] he1
    | B.isSingleton t1   = Right kit2
    | otherwise          = Msg.notNestRel [n] he1
    where
      lr     = [n] `B.headLR` B.headNames he1
      share  = B.headRShare lr
      he1'   = B.headMap share he1
      t1     = B.headNested he1'
      he2    = B.headUp he1'
      kit2   = C.relkitJust he2 $ C.RelkitOneToMany True kitf2
      kitf2  = B.relBody . C.gRel . head . share


-- ----------------------  chunk

--  > chunk /a /b /c

consChunk :: (Ord c, C.CRel c) => C.RopCons c
consChunk med =
  do ns  <- Op.getTerms med "-term"
     ord <- Op.getOption [] Op.getTerms med "-order"
     Right $ relmapChunk med ns ord

relmapChunk :: (Ord c, C.CRel c) => C.Intmed c -> [B.TermName] -> [B.TermName] -> C.Relmap c
relmapChunk med ns ord = C.relmapFlow med $ relkitChunk ns ord

relkitChunk :: (Ord c, C.CRel c) => [B.TermName] -> [B.TermName] -> C.RelkitFlow c
relkitChunk _ _ Nothing = Right C.relkitNothing
relkitChunk ns ord (Just he1) = Right kit2 where
    he2     = B.headNests ns he1
    kit2    = C.relkitJust he2 $ C.RelkitFull False f2
    f2 bo1  = let deg    = length bo1 `ceilingRem` length ns
                  bo1'   = B.sortByName (map B.Asc ord) ns bo1
                  ch     = B.chunks deg bo1'
                  rels   = (C.pRel . B.Rel he1) `map` ch
              in [rels]

ceilingRem :: (Integral a) => a -> a -> a
ceilingRem a b =
    case a `quotRem` b of
      (q, 0) -> q
      (q, _) -> q + 1


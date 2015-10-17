{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Nest.Flow
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
import qualified Koshucode.Baala.Data          as D
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Rop.Builtin   as Op
import qualified Koshucode.Baala.Rop.Message   as Msg



-- ----------------------  down

-- $DownExample
--
--  Enclose relation from @a@ into term @\/r@.
--  In other words, relation flows down to nested level.
--
--    > a | down /r

consDown :: (D.CRel c) => C.RopCons c
consDown med =
  do n <- Op.getTerm med "-term"
     Right $ relmapDown med n

relmapDown :: (D.CRel c) => C.Intmed c -> D.TermName -> C.Relmap c
relmapDown med = C.relmapFlow med . relkitDown

relkitDown :: (D.CRel c) => D.TermName -> C.RelkitFlow c
relkitDown _ Nothing = Right C.relkitNothing
relkitDown n (Just he1) = Right kit2 where
    he2       = D.headConsNest n he1 $ D.headEmpty
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = [[ D.pRel $ D.Rel he1 bo1 ]]



-- ----------------------  up

-- $UpExample
--
--  Lift nested relation @\/r@ up onto current flow.
--
--    > b | up /r

consUp :: (D.CRel c) => C.RopCons c
consUp med =
  do n <- Op.getTerm med "-term"
     Right $ relmapUp med n

relmapUp :: (D.CRel c) => C.Intmed c -> D.TermName -> C.Relmap c
relmapUp med = C.relmapFlow med . relkitUp

relkitUp :: (D.CRel c) => D.TermName -> C.RelkitFlow c
relkitUp _ Nothing = Right C.relkitNothing
relkitUp n (Just he1)
    | D.headDisjoint lr  = Msg.unkTerm [n] he1
    | B.isSingleton t1   = Right kit2
    | otherwise          = Msg.notNestRel [n] he1
    where
      lr     = [n] `D.headLR` D.headNames he1
      share  = D.headRShare lr
      he1'   = D.headMap share he1
      t1     = D.headNested he1'
      he2    = D.headUp he1'
      kit2   = C.relkitJust he2 $ C.RelkitOneToMany True kitf2
      kitf2  = D.relBody . D.gRel . head . share


-- ----------------------  chunk

--  > chunk /a /b /c

consChunk :: (Ord c, D.CRel c) => C.RopCons c
consChunk med =
  do ns  <- Op.getTerms med "-term"
     ord <- Op.getOption [] Op.getTerms med "-order"
     Right $ relmapChunk med ns ord

relmapChunk :: (Ord c, D.CRel c) => C.Intmed c -> [D.TermName] -> [D.TermName] -> C.Relmap c
relmapChunk med ns ord = C.relmapFlow med $ relkitChunk ns ord

relkitChunk :: (Ord c, D.CRel c) => [D.TermName] -> [D.TermName] -> C.RelkitFlow c
relkitChunk _ _ Nothing = Right C.relkitNothing
relkitChunk ns ord (Just he1) = Right kit2 where
    he2     = D.headNests ns he1
    kit2    = C.relkitJust he2 $ C.RelkitFull False f2
    f2 bo1  = let deg    = length bo1 `ceilingRem` length ns
                  bo1'   = B.sortByName (map B.Asc ord) ns bo1
                  ch     = B.chunks deg bo1'
                  rels   = (D.pRel . D.Rel he1) `map` ch
              in [rels]

ceilingRem :: (Integral a) => a -> a -> a
ceilingRem a b =
    case a `quotRem` b of
      (q, 0) -> q
      (q, _) -> q + 1


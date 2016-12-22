{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators concerning nested relation.

module Koshucode.Baala.Rop.Nest.Flow
  ( -- * down
    consDown, relmapDown, relkitDown,
    -- * up
    consUp, relmapUp, relkitUp,
    -- * chunk
    consChunk, relmapChunk, relkitChunk,
  ) where

import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Base.Message  as Msg


-- ----------------------  down

-- | __down \/N__
--
--   Enclose input relation at term @\/N@.
--   In other words, relation flows down to nested level.
--
consDown :: (K.CRel c) => C.RopCons c
consDown med =
  do n <- Rop.getTerm med "-term"
     Right $ relmapDown med n

-- | Create @down@ relmap.
relmapDown :: (K.CRel c) => C.Intmed c -> K.TermName -> C.Relmap c
relmapDown med = C.relmapFlow med . relkitDown

-- | Create @down@ relkit.
relkitDown :: (K.CRel c) => K.TermName -> C.RelkitFlow c
relkitDown _ Nothing = Right C.relkitNothing
relkitDown n (Just he1) = Right kit2 where
    he2       = K.headConsNest n he1 mempty
    kit2      = C.relkitFull he2 False flow
    flow bo1  = [[ K.pRel $ K.Rel he1 bo1 ]]


-- ----------------------  up

-- | __up \/P__
--
--   Lift nested relation @\/P@ up to current flow.
--
consUp :: (K.CRel c) => C.RopCons c
consUp med =
  do n <- Rop.getTerm med "-term"
     Right $ relmapUp med n

-- | Create @up@ relmap.
relmapUp :: (K.CRel c) => C.Intmed c -> K.TermName -> C.Relmap c
relmapUp med = C.relmapFlow med . relkitUp

-- | Create @up@ relkit.
relkitUp :: (K.CRel c) => K.TermName -> C.RelkitFlow c
relkitUp _ Nothing = Right C.relkitNothing
relkitUp n (Just he1)
    | K.newTermsExist pk  = Msg.newTerm pk he1
    | K.isSingleton t1    = Right kit2
    | otherwise           = Msg.notNestRel [n] he1
    where
      pk     = K.termPicker [n] he1
      pick   = K.pickTerms pk
      he1'   = K.headMap pick he1
      t1     = K.headNested he1'
      he2    = K.headUp he1'
      kit2   = C.relkitMany he2 True flow
      flow   = K.relBody . K.gRel . head . pick


-- ----------------------  chunk

-- | __chunk \/N ... -order \/P ...__
--
--   Split input relation into multiple chunks named \/N ....
--   The input relation is ordered by \/P ....
--
consChunk :: (Ord c, K.CRel c) => C.RopCons c
consChunk med =
  do ns  <- Rop.getTerms med "-term"
     ord <- Rop.getOption [] Rop.getTerms med "-order"
     Right $ relmapChunk med ns ord

-- | Create @chunk@ relmap.
relmapChunk :: (Ord c, K.CRel c) => C.Intmed c -> [K.TermName] -> [K.TermName] -> C.Relmap c
relmapChunk med ns ord = C.relmapFlow med $ relkitChunk ns ord

-- | Create @chunk@ relkit.
relkitChunk :: (Ord c, K.CRel c) => [K.TermName] -> [K.TermName] -> C.RelkitFlow c
relkitChunk _ _ Nothing = Right C.relkitNothing
relkitChunk ns ord (Just he1) = Right kit2 where
    he2       = K.headNests ns he1
    kit2      = C.relkitFull he2 False flow
    flow bo1  = let deg    = length bo1 `ceilingRem` length ns
                    bo1'   = K.sortByName (map K.Asc ord) ns bo1
                    ch     = K.chunks deg bo1'
                    rels   = (K.pRel . K.Rel he1) `map` ch
                in [rels]

ceilingRem :: (Integral a) => a -> a -> a
ceilingRem a b =
    case a `quotRem` b of
      (q, 0) -> q
      (q, _) -> q + 1


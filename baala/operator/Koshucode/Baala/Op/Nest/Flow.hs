{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Nest.Flow
( 
  -- * rel-down
  consRelDown, relmapRelDown, relkitRelDown,
  -- * rel-up
  consRelUp, relmapRelUp, relkitRelUp,
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Message as Message



-- ----------------------  rel-down

consRelDown :: (C.CRel c) => C.RopCons c
consRelDown use =
  do n <- Op.getTerm use "-term"
     Right $ relmapRelDown use n

relmapRelDown :: (C.CRel c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapRelDown use = C.relmapFlow use . relkitRelDown

relkitRelDown :: (C.CRel c) => B.TermName -> C.RelkitCalc c
relkitRelDown _ Nothing = Right C.relkitNothing
relkitRelDown n (Just he1) = Right kit2 where
    he2       = B.Relhead [B.Relnest n $ B.headTerms he1]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = [[ C.pRel $ B.Rel he1 bo1 ]]



-- ----------------------  rel-up

consRelUp :: (C.CRel c) => C.RopCons c
consRelUp use =
  do n <- Op.getTerm use "-term"
     Right $ relmapRelUp use n

relmapRelUp :: (C.CRel c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapRelUp use = C.relmapFlow use . relkitRelUp

relkitRelUp :: (C.CRel c) => B.TermName -> C.RelkitCalc c
relkitRelUp _ Nothing = Right C.relkitNothing
relkitRelUp n (Just he1)
    | null ind1     = Message.unkTerm [n] he1
    | B.isNested t1 = Right kit2
    | otherwise     = Message.notNestRel [n] he1
    where
      ns1   = B.headNames he1
      ind1  = [n] `B.snipIndex` ns1
      pick1 = B.snipFrom ind1
      t1    = head $ pick1 $ B.headTerms he1
      he2   = B.Relhead $ B.relnestTerms t1
      kit2  = C.relkitJust he2 $ C.RelkitOneToMany True kitf2
      kitf2 = B.relBody . C.gRel . head . pick1


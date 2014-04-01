{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Nest.Flow
( 
  -- * down
  consDown, relmapDown, relkitDown,
  -- $DownExample

  -- * up
  consUp, relmapUp, relkitUp,
  -- $UpExample
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Message as Message



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

relkitDown :: (C.CRel c) => B.TermName -> C.RelkitCalc c
relkitDown _ Nothing = Right C.relkitNothing
relkitDown n (Just he1) = Right kit2 where
    he2       = B.Relhead [B.Relnest n $ B.headTerms he1]
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

relkitUp :: (C.CRel c) => B.TermName -> C.RelkitCalc c
relkitUp _ Nothing = Right C.relkitNothing
relkitUp n (Just he1)
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


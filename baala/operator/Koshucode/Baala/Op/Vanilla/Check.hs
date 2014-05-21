{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Vanilla.Check
( 
  -- * check-term
  consCheckTerm,
  relmapCheckTermJust, relmapCheckTermHas, relmapCheckTermBut,
  relkitCheckTermJust, relkitCheckTermHas, relkitCheckTermBut,

  -- * duplicate
  -- $duplicate
  consDuplicate, relmapDuplicate,

  -- * typename
  consTypename,

  -- * dump
  consDump,
) where

import qualified Data.Map                    as Map
import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Builtin  as Op
import qualified Koshucode.Baala.Op.Message  as Message



-- ----------------------  check-term

consCheckTerm :: C.RopCons c
consCheckTerm use =
  do optJust <- Op.getMaybe Op.getTerms use "-just"
     optHas  <- Op.getMaybe Op.getTerms use "-has"
     optBut  <- Op.getMaybe Op.getTerms use "-but"
     case (optJust, optHas, optBut) of
       (Just ns, Nothing, Nothing) -> Right $ relmapCheckTermJust use ns
       (Nothing, Just ns, Nothing) -> Right $ relmapCheckTermHas  use ns
       (Nothing, Nothing, Just ns) -> Right $ relmapCheckTermBut  use ns
       _ -> Message.unexpAttr "require one of -just / -has / -but"

relmapCheckTermJust :: C.RopUse c -> [B.TermName] -> C.Relmap c
relmapCheckTermHas  :: C.RopUse c -> [B.TermName] -> C.Relmap c
relmapCheckTermBut  :: C.RopUse c -> [B.TermName] -> C.Relmap c
relmapCheckTermJust use = C.relmapFlow use . relkitCheckTermJust
relmapCheckTermHas  use = C.relmapFlow use . relkitCheckTermHas
relmapCheckTermBut  use = C.relmapFlow use . relkitCheckTermBut

relkitCheckTermJust :: [B.TermName] -> C.RelkitCalc c
relkitCheckTermHas  :: [B.TermName] -> C.RelkitCalc c
relkitCheckTermBut  :: [B.TermName] -> C.RelkitCalc c
relkitCheckTermJust = checkTerm "Just" (\ns he1 -> B.headFrom ns `B.headEquiv` he1)
relkitCheckTermHas  = checkTerm "Has"  (\ns he1 -> B.headFrom ns `B.isSubhead` he1)
relkitCheckTermBut  = checkTerm "But"  (\ns he1 -> null $ ns `B.snipShare` B.headNames he1)

checkTerm :: String -> ([B.TermName] -> B.Relhead -> Bool) -> [B.TermName] -> C.RelkitCalc c
checkTerm _ _ _ Nothing = Right C.relkitNothing
checkTerm opt check ns (Just he1)
    | check ns he1 = Right $ C.relkitJust he1 C.RelkitId
    | otherwise    = Message.checkTerm opt ns he1



-- ----------------------  duplicate

-- $duplicate
--
--  Output tuples of which key is duplicated.
--  Relmap @duplicate@ @\/x@ @\/y@ means
--  if set of terms @\/x@ and @\/y@ is a key of relation,
--  there are another tuples that has the same key.

consDuplicate :: (Ord c) => C.RopCons c
consDuplicate use =
  do ns <- Op.getTerms use "-term"
     Right $ relmapDuplicate use ns

relmapDuplicate :: (Ord c) => C.RopUse c -> [B.TermName] -> C.Relmap c
relmapDuplicate use = C.relmapFlow use . relkitDuplicate

relkitDuplicate :: (Ord c) => [B.TermName] -> C.RelkitCalc c
relkitDuplicate _ Nothing = Right C.relkitNothing
relkitDuplicate ns (Just he1)
    | null nsLeft = Right kit2
    | otherwise   = Message.unkTerm nsLeft he1
    where
      nsLeft :: [B.TermName]
      nsLeft = ns `B.snipLeft` ns1

      ns1    = B.headNames he1
      ind1   = ns `B.snipIndex` ns1
      share1 = B.snipFrom ind1

      kit2 = C.relkitJust he1 $ C.RelkitFull False kitf2
      kitf2 :: (Ord c) => [[c]] -> [[c]]
      kitf2 bo1 = let bo1map = B.gatherToMap $ map kv bo1
                  in concat $ Map.elems $ Map.filter dup bo1map
      kv cs1    = (share1 cs1, cs1)
      dup       = not . B.isSingleton



-- ----------------------  typename

-- | Get typename.
consTypename :: (C.CContent c) => C.RopCons c
consTypename use =
  do np <- Op.getTermPairs use "-term"
     Right $ C.relmapFlow use $ relkitTypename np

relkitTypename :: (C.CText c) => [B.TermName2] -> C.RelkitCalc c
relkitTypename _ Nothing = Right C.relkitNothing
relkitTypename np (Just he1) = Right kit2 where
    ns, ps :: [B.TermName]
    (ns, ps)  = unzip np

    ns1       = B.headNames he1
    ind1      = ps `B.snipIndex` ns1
    share1    = B.snipFrom ind1

    he2       = ns `B.headAppend` he1
    kit2      = C.relkitJust he2 $ C.RelkitOneToOne False kitf2
    kitf2 cs1 = let cs2 = share1 cs1 in map typetext cs2 ++ cs1
    typetext  = C.pText . C.typename


-- ----------------------  dump

consDump :: (B.Write c, C.CRel c) => C.RopCons c
consDump use = Right $ C.relmapFlow use $ relkitDump

relkitDump :: (B.Write c, C.CRel c) => C.RelkitCalc c
relkitDump Nothing = Right C.relkitNothing
relkitDump (Just he1) = Right kit2 where
    kit2 = C.relkitJust he1 $ C.RelkitAbFull False kitf2 []
    kitf2 _ bo1 = Message.dumpRel $ B.Rel he1 bo1

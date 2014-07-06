{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Check
( ropsCheck,
) where

import qualified Data.Map                    as Map
import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Builtin  as Op
import qualified Koshucode.Baala.Op.Lattice  as Op
import qualified Koshucode.Baala.Op.Term     as Op
import qualified Koshucode.Baala.Op.Message  as Message


-- | Implementation of relational operators.
--
--   [@check-term \[ -just \/P ... | -has \/P ... | -but \/N ... \]@]
--     Check occurences of terms for input relation.
--
--   [@duplicate \/P ...@]
--     Pass duplicate tuples on @\/P@ ...

ropsCheck :: (C.CContent c) => [C.Rop c]
ropsCheck = Op.ropList "check"
    [ Op.ropN consCheckTerm  "check-term [-just /N ... | -has /N ... | -but /N ...]"
                                                       "| -just -has -but"
    , Op.ropN consDump       "dump"                    ""
    , Op.ropV consDuplicate  "duplicate /N ..."        "-term"
    , Op.ropV consExclude    "exclude /N ... -from R"  "-term | -from/"
    ]


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

relkitCheckTermJust :: [B.TermName] -> C.RelkitFlow c
relkitCheckTermHas  :: [B.TermName] -> C.RelkitFlow c
relkitCheckTermBut  :: [B.TermName] -> C.RelkitFlow c
relkitCheckTermJust = checkTerm "Just" (\ns he1 -> B.headFrom ns `B.headEquiv` he1)
relkitCheckTermHas  = checkTerm "Has"  (\ns he1 -> B.headFrom ns `B.isSubhead` he1)
relkitCheckTermBut  = checkTerm "But"  (\ns he1 -> null $ ns `B.snipShare` B.headNames he1)

checkTerm :: String -> ([B.TermName] -> B.Relhead -> Bool) -> [B.TermName] -> C.RelkitFlow c
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

relkitDuplicate :: (Ord c) => [B.TermName] -> C.RelkitFlow c
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



-- ----------------------  exclude

-- exclude : none ( pick @'all | meet ( @from | pick @'all ))

consExclude :: (Ord c) => C.RopCons c
consExclude use =
  do ns <- Op.getTerms  use "-term"
     m  <- Op.getRelmap use "-from"
     Right $ relmapExclude use (ns, m)

relmapExclude :: (Ord c) => C.RopUse c -> ([B.TermName], C.Relmap c) -> C.Relmap c
relmapExclude use (ns, m) = excl where
    excl = Op.relmapNone use (pick `B.mappend` meet)
    pick = Op.relmapPick use ns
    meet = Op.relmapMeet use (m `B.mappend` pick)


-- ----------------------  dump

consDump :: (B.Write c, C.CRel c) => C.RopCons c
consDump use = Right $ C.relmapFlow use $ relkitDump

relkitDump :: (B.Write c, C.CRel c) => C.RelkitFlow c
relkitDump Nothing = Right C.relkitNothing
relkitDump (Just he1) = Right kit2 where
    kit2 = C.relkitJust he1 $ C.RelkitAbFull False kitf2 []
    kitf2 _ bo1 = Message.dumpRel $ B.Rel he1 bo1


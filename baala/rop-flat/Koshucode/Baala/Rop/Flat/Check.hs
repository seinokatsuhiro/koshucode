{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Flat.Check
  ( ropsCheck,
  ) where

import qualified Data.Map                     as Map
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Syntax       as D
import qualified Koshucode.Baala.Data         as D
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Rop.Base     as Op
import qualified Koshucode.Baala.Rop.Flat.Lattice  as Op
import qualified Koshucode.Baala.Rop.Flat.Term     as Op
import qualified Koshucode.Baala.Rop.Flat.Message  as Msg


-- | Implementation of relational operators.
--
--   [@check-term \[ -just \/P ... | -has \/P ... | -but \/N ... \]@]
--     Check occurences of terms for input relation.
--
--   [@duplicate \/P ...@]
--     Pass duplicate tuples on @\/P@ ...

ropsCheck :: (D.CContent c) => [C.Rop c]
ropsCheck = Op.ropList "check"
    [ Op.def consCheckTerm  "check-term [-just /N ... | -has /N ... | -but /N ...]"
                                                      "0 | -just -has -but"
    , Op.def consDump       "dump"                    "0"
    , Op.def consDuplicate  "duplicate /N ..."        "V -term"
    , Op.def consExclude    "exclude /N ... -from R"  "V -term | -from/"
    ]


-- ----------------------  check-term

consCheckTerm :: C.RopCons c
consCheckTerm med =
  do optJust <- Op.getMaybe Op.getTerms med "-just"
     optHas  <- Op.getMaybe Op.getTerms med "-has"
     optBut  <- Op.getMaybe Op.getTerms med "-but"
     case (optJust, optHas, optBut) of
       (Just ns, Nothing, Nothing) -> Right $ relmapCheckTermJust med ns
       (Nothing, Just ns, Nothing) -> Right $ relmapCheckTermHas  med ns
       (Nothing, Nothing, Just ns) -> Right $ relmapCheckTermBut  med ns
       _ -> Msg.unexpAttr "require one of -just / -has / -but"

relmapCheckTermJust :: C.Intmed c -> [D.TermName] -> C.Relmap c
relmapCheckTermHas  :: C.Intmed c -> [D.TermName] -> C.Relmap c
relmapCheckTermBut  :: C.Intmed c -> [D.TermName] -> C.Relmap c
relmapCheckTermJust med = C.relmapFlow med . relkitCheckTermJust
relmapCheckTermHas  med = C.relmapFlow med . relkitCheckTermHas
relmapCheckTermBut  med = C.relmapFlow med . relkitCheckTermBut

relkitCheckTermJust :: [D.TermName] -> C.RelkitFlow c
relkitCheckTermHas  :: [D.TermName] -> C.RelkitFlow c
relkitCheckTermBut  :: [D.TermName] -> C.RelkitFlow c
relkitCheckTermJust = checkTerm "Just" (\ns he1 -> D.headFrom ns `D.headEquiv` he1)
relkitCheckTermHas  = checkTerm "Has"  (\ns he1 -> D.headFrom ns `D.isSubhead` he1)
relkitCheckTermBut  = checkTerm "But"  (\ns he1 -> null $ ns `B.snipShare` D.headNames he1)

checkTerm :: String -> ([D.TermName] -> D.Head -> Bool) -> [D.TermName] -> C.RelkitFlow c
checkTerm _ _ _ Nothing = Right C.relkitNothing
checkTerm opt check ns (Just he1)
    | check ns he1 = Right $ C.relkitJust he1 C.RelkitId
    | otherwise    = Msg.checkTerm opt ns he1



-- ----------------------  duplicate

-- $duplicate
--
--  Output tuples of which key is duplicated.
--  Relmap @duplicate@ @\/x@ @\/y@ means
--  if set of terms @\/x@ and @\/y@ is a key of relation,
--  there are another tuples that has the same key.

consDuplicate :: (Ord c) => C.RopCons c
consDuplicate med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapDuplicate med ns

relmapDuplicate :: (Ord c) => C.Intmed c -> [D.TermName] -> C.Relmap c
relmapDuplicate med = C.relmapFlow med . relkitDuplicate

relkitDuplicate :: (Ord c) => [D.TermName] -> C.RelkitFlow c
relkitDuplicate _ Nothing = Right C.relkitNothing
relkitDuplicate ns (Just he1)
    | null unk   = Right kit2
    | otherwise  = Msg.unkTerm unk he1
    where
      lr     = ns `D.headLR` D.headNames he1
      unk    = D.headLSideNames lr
      kit2   = C.relkitJust he1 $ C.RelkitFull False kitf2
      dup    = not . B.isSingleton

      kitf2 :: (Ord c) => [[c]] -> [[c]]
      kitf2 bo1 = let bo1map = B.gatherToMap $ map (D.headRAssoc lr) bo1
                  in concat $ Map.elems $ Map.filter dup bo1map



-- ----------------------  exclude

-- exclude : none ( pick @'all | meet ( @from | pick @'all ))

consExclude :: (Ord c) => C.RopCons c
consExclude med =
  do ns <- Op.getTerms  med "-term"
     m  <- Op.getRelmap med "-from"
     Right $ relmapExclude med (ns, m)

relmapExclude :: (Ord c) => C.Intmed c -> ([D.TermName], C.Relmap c) -> C.Relmap c
relmapExclude med (ns, m) = excl where
    excl = Op.relmapNone med (pick `B.mappend` meet)
    pick = Op.relmapPick med ns
    meet = Op.relmapMeet med (m `B.mappend` pick)


-- ----------------------  dump

consDump :: (B.Write c, D.CRel c) => C.RopCons c
consDump med = Right $ C.relmapFlow med $ relkitDump

relkitDump :: (B.Write c, D.CRel c) => C.RelkitFlow c
relkitDump Nothing = Right C.relkitNothing
relkitDump (Just he1) = Right kit2 where
    kit2 = C.relkitJust he1 $ C.RelkitAbFull False kitf2 []
    kitf2 _ bo1 = Msg.dumpRel $ D.Rel he1 bo1


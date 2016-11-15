{-# OPTIONS_GHC -Wall #-}

-- | Check data property.

module Koshucode.Baala.Rop.Flat.Check
  ( ropsCheck,
    consCheckTerm,
    consDuplicate,
    consExclude,
    consDump,
  ) where

import qualified Data.Map                          as Map
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S
import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Flat.Lattice  as Rop
import qualified Koshucode.Baala.Rop.Flat.Term     as Rop
import qualified Koshucode.Baala.Rop.Flat.Message  as Msg

-- | Implementation of relational operators.
ropsCheck :: (D.CContent c) => [C.Rop c]
ropsCheck = Rop.ropList "check"
    [ Rop.def consCheckTerm  "check-term [-just /N ... | -has /N ... | -but /N ...]"
                             "just : . -just | has : . -has | but : . -but"
    , Rop.def consDump       "dump"                    ""
    , Rop.def consDuplicate  "duplicate /N ..."        "-term*"
    , Rop.def consExclude    "exclude /N ... -from R"  "-term* . -from/"
    ]


-- ----------------------  check-term

-- | [check-term -just \/P ...]
--   [check-term -has \/P ...]
--   [check-term -but \/N ...]
consCheckTerm :: C.RopCons c
consCheckTerm med =
  case Rop.getTag med of
    tag | tag "just" -> call relmapCheckTermJust "-just"
        | tag "has"  -> call relmapCheckTermHas  "-has"
        | tag "but"  -> call relmapCheckTermBut  "-but"
        | otherwise  -> B.bug "check-term"
    where call f a = do ns <- Rop.getTerms med a
                        Right $ f med ns

relmapCheckTermJust :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapCheckTermHas  :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapCheckTermBut  :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapCheckTermJust med = C.relmapFlow med . relkitCheckTermJust
relmapCheckTermHas  med = C.relmapFlow med . relkitCheckTermHas
relmapCheckTermBut  med = C.relmapFlow med . relkitCheckTermBut

relkitCheckTermJust :: [S.TermName] -> C.RelkitFlow c
relkitCheckTermHas  :: [S.TermName] -> C.RelkitFlow c
relkitCheckTermBut  :: [S.TermName] -> C.RelkitFlow c
relkitCheckTermJust = checkTerm "Just" (\ns he1 -> D.headFrom ns `D.headEquiv` he1)
relkitCheckTermHas  = checkTerm "Has"  (\ns he1 -> D.headFrom ns `D.isSubhead` he1)
relkitCheckTermBut  = checkTerm "But"  (\ns he1 -> null $ ns `B.snipShare` D.getTermNames he1)

checkTerm :: String -> ([S.TermName] -> D.Head -> Bool) -> [S.TermName] -> C.RelkitFlow c
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

-- | __duplicate \/P ...__
--
--   Output tuples duplicate on terms /\/P .../.
--
consDuplicate :: (Ord c) => C.RopCons c
consDuplicate med =
  do ns <- Rop.getTerms med "-term"
     Right $ relmapDuplicate med ns

relmapDuplicate :: (Ord c) => C.Intmed c -> [S.TermName] -> C.Relmap c
relmapDuplicate med = C.relmapFlow med . relkitDuplicate

relkitDuplicate :: (Ord c) => [S.TermName] -> C.RelkitFlow c
relkitDuplicate _ Nothing = Right C.relkitNothing
relkitDuplicate ns (Just he1)
    | D.newTermsExist pk  = Msg.unkTerm (D.newTerms pk) he1
    | otherwise           = Right kit2
    where
      pk     = D.termPicker ns he1
      kit2   = C.relkitJust he1 $ C.RelkitFull False kitf2
      dup    = not . B.isSingleton

      kitf2 :: (Ord c) => [[c]] -> [[c]]
      kitf2 bo1 = let bo1map = B.gatherToMap $ map (D.ssRAssoc pk) bo1
                  in concat $ Map.elems $ Map.filter dup bo1map


-- ----------------------  exclude

-- exclude : none ( pick @'all | meet ( @from | pick @'all ))

-- | __exclude \/N ... -from R__
consExclude :: (Ord c) => C.RopCons c
consExclude med =
  do ns <- Rop.getTerms  med "-term"
     m  <- Rop.getRelmap med "-from"
     Right $ relmapExclude med (ns, m)

relmapExclude :: (Ord c) => C.Intmed c -> ([S.TermName], C.Relmap c) -> C.Relmap c
relmapExclude med (ns, m) = excl where
    excl = Rop.relmapNone med (pick B.<> meet)
    pick = Rop.relmapPick med ns
    meet = Rop.relmapMeet med Nothing (m B.<> pick)


-- ----------------------  dump

-- | __dump__
--
--   Dump input relation and abort.
--
consDump :: (D.CRel c, B.MixShortEncode c) => C.RopCons c
consDump med = Right $ C.relmapFlow med $ relkitDump

relkitDump :: (D.CRel c, B.MixShortEncode c) => C.RelkitFlow c
relkitDump Nothing = Right C.relkitNothing
relkitDump (Just he1) = Right kit2 where
    kit2 = C.relkitJust he1 $ C.RelkitAbFull False kitf2 []
    kitf2 _ bo1 = Msg.dumpRel $ D.Rel he1 bo1


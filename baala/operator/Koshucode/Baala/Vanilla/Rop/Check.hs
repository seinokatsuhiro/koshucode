{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Check
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
) where

import qualified Data.Map                 as Map
import qualified Koshucode.Baala.Base     as B
import qualified Koshucode.Baala.Core     as C
import qualified Koshucode.Baala.Builtin  as Rop



-- ----------------------  check-term

consCheckTerm :: C.RopCons c
consCheckTerm use =
  do optJust <- Rop.getMaybe Rop.getTerms use "-just"
     optHas  <- Rop.getMaybe Rop.getTerms use "-has"
     optBut  <- Rop.getMaybe Rop.getTerms use "-but"
     case (optJust, optHas, optBut) of
       (Just ns, Nothing, Nothing) -> Right $ relmapCheckTermJust use ns
       (Nothing, Just ns, Nothing) -> Right $ relmapCheckTermHas  use ns
       (Nothing, Nothing, Just ns) -> Right $ relmapCheckTermBut  use ns
       _ -> Left $ B.abortOperand "require one of -just / -has / -but"

relmapCheckTermJust :: C.RopUse c -> [B.Termname] -> C.Relmap c
relmapCheckTermHas  :: C.RopUse c -> [B.Termname] -> C.Relmap c
relmapCheckTermBut  :: C.RopUse c -> [B.Termname] -> C.Relmap c
relmapCheckTermJust use = C.relmapFlow use . relkitCheckTermJust
relmapCheckTermHas  use = C.relmapFlow use . relkitCheckTermHas
relmapCheckTermBut  use = C.relmapFlow use . relkitCheckTermBut

relkitCheckTermJust :: [B.Termname] -> C.RelkitCalc c
relkitCheckTermHas  :: [B.Termname] -> C.RelkitCalc c
relkitCheckTermBut  :: [B.Termname] -> C.RelkitCalc c
relkitCheckTermJust = relkitCheckTermBy (\ns he1 -> B.headFrom ns `B.isEqvHead` he1)
relkitCheckTermHas  = relkitCheckTermBy (\ns he1 -> B.headFrom ns `B.isSubhead` he1)
relkitCheckTermBut  = relkitCheckTermBy (\ns he1 -> null $ B.headKeepTerms he1 ns)

relkitCheckTermBy :: ([String] -> B.Relhead -> Bool) -> [String] -> C.RelkitCalc c
relkitCheckTermBy _ _ Nothing = Right C.relkitNothing
relkitCheckTermBy f ns (Just he1)
    | f ns he1 = Right $ C.relkitJust he1 C.RelkitId
    | otherwise = Left $ B.AbortAnalysis [] (B.AACheckTerms $ B.headNames he1)



-- ----------------------  duplicate

{- $duplicate

   Output tuples of which key is duplicated.
   Relmap @duplicate@ @\/x@ @\/y@ means
   if set of terms @\/x@ and @\/y@ is a key of relation,
   there are another tuples that has the same key.

-}  

consDuplicate :: (Ord c) => C.RopCons c
consDuplicate use =
  do ns <- Rop.getTerms use "-term"
     Right $ relmapDuplicate use ns

relmapDuplicate :: (Ord c) => C.RopUse c -> [B.Termname] -> C.Relmap c
relmapDuplicate use = C.relmapFlow use . relkitDuplicate

relkitDuplicate :: (Ord c) => [B.Termname] -> C.RelkitCalc c
relkitDuplicate _ Nothing = Right C.relkitNothing
relkitDuplicate ns (Just he1)
    | null non  = Right kit2
    | otherwise = Left  $ B.AbortAnalysis [] (B.AANoTerms non)
    where
      non :: [B.Termname]
      non = B.headDropTerms he1 ns

      pos :: [B.TermPos]
      pos = he1 `B.posFor` ns

      kit2 = C.relkitJust he1 $ C.RelkitFull False kitf2
      kitf2 :: (Ord c) => [[c]] -> [[c]]
      kitf2 bo1 = let bo1map = B.gatherToMap $ map kv bo1
                  in concat $ Map.elems $ Map.filter dup bo1map
      kv cs     = (B.posPick pos cs, cs)
      dup       = not . B.isSingleton



-- ----------------------  typename

{-| Get typename. -}
consTypename :: (C.CContent c) => C.RopCons c
consTypename use =
  do np <- Rop.getTermPairs use "-term"
     Right $ C.relmapFlow use $ relkitTypename np

relkitTypename
  :: (C.CText c) => [(B.Termname, B.Termname)] -> C.RelkitCalc c
relkitTypename _ Nothing = Right C.relkitNothing
relkitTypename np (Just he1) = Right kit2 where
    ns    = map fst np
    ps    = map snd np
    he2   = B.headAppend ns he1
    pos   = he1 `B.posFor` ps

    kit2 = C.relkitJust he2 $ C.RelkitOneToOne False kitf2
    kitf2 cs1 = let cs2 = B.posPick pos cs1
                in ((C.pText . C.typename) `map` cs2) ++ cs1


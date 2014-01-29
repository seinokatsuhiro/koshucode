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
       _ -> Left $ B.AbortAnalysis [] $ B.AAMalformedOperand "require one of -just / -has / -but"

relmapCheckTermJust :: C.RopUse c -> [B.Termname] -> C.Relmap c
relmapCheckTermHas  :: C.RopUse c -> [B.Termname] -> C.Relmap c
relmapCheckTermBut  :: C.RopUse c -> [B.Termname] -> C.Relmap c
relmapCheckTermJust use = C.relmapCalc use . relkitCheckTermJust
relmapCheckTermHas  use = C.relmapCalc use . relkitCheckTermHas
relmapCheckTermBut  use = C.relmapCalc use . relkitCheckTermBut

relkitCheckTermJust :: [B.Termname] -> B.Relhead -> B.Ab (C.Relkit c)
relkitCheckTermHas  :: [B.Termname] -> B.Relhead -> B.Ab (C.Relkit c)
relkitCheckTermBut  :: [B.Termname] -> B.Relhead -> B.Ab (C.Relkit c)
relkitCheckTermJust = relkitCheckTermBy (\ns h1 -> B.headFrom ns `B.isEqvHead` h1)
relkitCheckTermHas  = relkitCheckTermBy (\ns h1 -> B.headFrom ns `B.isSubhead` h1)
relkitCheckTermBut  = relkitCheckTermBy (\ns h1 -> null $ B.headKeepTerms h1 ns)

relkitCheckTermBy :: ([String] -> B.Relhead -> Bool)
                 -> [String] -> B.Relhead -> B.Ab (C.Relkit c)
relkitCheckTermBy f ns h1
    | f ns h1 = Right $ C.relkit h1 C.RelkitId
    | otherwise = Left $ B.AbortAnalysis [] (B.AACheckTerms $ B.headNames h1)



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
relmapDuplicate use ns = C.relmapCalc use $ relkitDuplicate ns

relkitDuplicate
  :: (Ord c) => [B.Termname] -> B.Relhead -> B.Ab (C.Relkit c)
relkitDuplicate ns h1
    | null non  = Right $ C.relkit h1 (C.RelkitFull False f)
    | otherwise = Left  $ B.AbortAnalysis [] (B.AANoTerms non)
    where
      non :: [B.Termname]
      non = B.headDropTerms h1 ns

      pos :: [B.TermPos]
      pos = h1 `B.posFor` ns

      f :: (Ord c) => [[c]] -> [[c]]
      f b1    = let m = B.gatherToMap $ map kv b1
                in concat $ Map.elems $ Map.filter dup m
      kv cs1  = ( B.posPick pos cs1, cs1 )
      dup     = not . B.isSingleton



-- ----------------------  typename

{-| Get typename. -}
consTypename :: (C.CContent c) => C.RopCons c
consTypename use =
  do (n, p) <- Rop.getTermPair use "-term"
     Right $ C.relmapCalc use $ relkitTypename (n, p)

relkitTypename
  :: (C.CText c) => (B.Termname, B.Termname) -> B.Relhead -> B.Ab (C.Relkit c)
relkitTypename (n, p) h1 = Right $ C.relkit h2 (C.RelkitOneToOne False f) where
    h2    = B.headCons n h1
    pos   = h1 `B.posFor` [p]
    f cs1 = let [c] = B.posPick pos cs1
            in C.putText (C.typename c) : cs1


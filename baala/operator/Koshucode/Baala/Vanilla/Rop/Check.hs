{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Check
( 
  -- * check-term
  ropConsCheckTerm,
  relmapCheckTermJust, relmapCheckTermHas, relmapCheckTermBut,
  relfyCheckTermJust, relfyCheckTermHas, relfyCheckTermBut,

  -- * duplicate
  -- $duplicate
  ropConsDuplicate, relmapDuplicate,

  -- * typename
  ropConsTypename,
) where

import qualified Data.Map                 as Map
import qualified Koshucode.Baala.Base     as B
import qualified Koshucode.Baala.Core     as C
import qualified Koshucode.Baala.Builtin  as Rop



-- ----------------------  check-term

ropConsCheckTerm :: C.RopCons c
ropConsCheckTerm use =
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
relmapCheckTermJust use = C.relmapCalc use . relfyCheckTermJust
relmapCheckTermHas  use = C.relmapCalc use . relfyCheckTermHas
relmapCheckTermBut  use = C.relmapCalc use . relfyCheckTermBut

relfyCheckTermJust :: [B.Termname] -> B.Relhead -> B.Ab (C.Relfy c)
relfyCheckTermHas  :: [B.Termname] -> B.Relhead -> B.Ab (C.Relfy c)
relfyCheckTermBut  :: [B.Termname] -> B.Relhead -> B.Ab (C.Relfy c)
relfyCheckTermJust = relfyCheckTermBy (\ns h1 -> B.headFrom ns `B.isEqvHead` h1)
relfyCheckTermHas  = relfyCheckTermBy (\ns h1 -> B.headFrom ns `B.isSubhead` h1)
relfyCheckTermBut  = relfyCheckTermBy (\ns h1 -> null $ B.headKeepTerms h1 ns)

relfyCheckTermBy :: ([String] -> B.Relhead -> Bool)
                 -> [String] -> B.Relhead -> B.Ab (C.Relfy c)
relfyCheckTermBy f ns h1
    | f ns h1 = Right $ C.relfy h1 C.RelfyId
    | otherwise = Left $ B.AbortAnalysis [] (B.AACheckTerms $ B.headNames h1)



-- ----------------------  duplicate

{- $duplicate

   Output tuples of which key is duplicated.
   Relmap @duplicate@ @\/x@ @\/y@ means
   if set of terms @\/x@ and @\/y@ is a key of relation,
   there are another tuples that has the same key.

-}  

ropConsDuplicate :: (Ord c) => C.RopCons c
ropConsDuplicate use =
  do ns <- Rop.getTerms use "-term"
     Right $ relmapDuplicate use ns

relmapDuplicate :: (Ord c) => C.RopUse c -> [B.Termname] -> C.Relmap c
relmapDuplicate use ns = C.relmapCalc use $ relfyDuplicate ns

relfyDuplicate
  :: (Ord c) => [B.Termname] -> B.Relhead -> B.Ab (C.Relfy c)
relfyDuplicate ns h1
    | null non  = Right $ C.relfy h1 (C.RelfyFull False f)
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
ropConsTypename :: (C.CContent c) => C.RopCons c
ropConsTypename use =
  do (n, p) <- Rop.getTermPair use "-term"
     Right $ C.relmapCalc use $ relfyTypename (n, p)

relfyTypename
  :: (C.CText c) => (B.Termname, B.Termname) -> B.Relhead -> B.Ab (C.Relfy c)
relfyTypename (n, p) h1 = Right $ C.relfy h2 (C.RelfyOneToOne False f) where
    h2    = B.headCons n h1
    pos   = h1 `B.posFor` [p]
    f cs1 = let [c] = B.posPick pos cs1
            in C.putText (C.typename c) : cs1


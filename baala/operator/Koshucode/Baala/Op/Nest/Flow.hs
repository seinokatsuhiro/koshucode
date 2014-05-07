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

  -- * join-up
  consJoinUp, relmapJoinUp,

  -- * split
  consSplit, relmapSplit, relkitSplit,
) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Op.Builtin    as Op
import qualified Koshucode.Baala.Op.Minimal    as Op
import qualified Koshucode.Baala.Op.Message    as Message



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
    he2       = B.Relhead [B.TermNest n $ B.headTerms he1]
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
    | null ind1       = Message.unkTerm [n] he1
    | B.isTermNest t1 = Right kit2
    | otherwise       = Message.notNestRel [n] he1
    where
      ns1   = B.headNames he1
      ind1  = [n] `B.snipIndex` ns1
      pick1 = B.snipFrom ind1
      t1    = head $ pick1 $ B.headTerms he1
      he2   = B.Relhead $ B.termNest t1
      kit2  = C.relkitJust he2 $ C.RelkitOneToMany True kitf2
      kitf2 = B.relBody . C.gRel . head . pick1



-- ----------------------  join-up

consJoinUp :: (Ord c) => C.RopCons c
consJoinUp use =
  do with <- Op.getWithTerms use "-term"
     Right $ relmapJoinUp use with

relmapJoinUp :: (Ord c) => C.RopUse c -> [B.Terminal String] -> C.Relmap c
relmapJoinUp use with = C.relmapWith use with $ Op.relmapJoinList use rmaps where
    rmaps = link `map` map snd with
    link v = C.relmapLink use v []



-- ----------------------  split

consSplit :: (C.CContent c) => C.RopCons c
consSplit use =
    do treesLet <- Op.getOption [] Op.getWordTrees use "-let"
       treesIn  <- Op.getTermTrees use "-in"
       coxLet   <- alphas use treesLet
       coxIn    <- alphas use treesIn
       let base = C.globalFunction $ C.ropGlobal use
       Right $ relmapSplit use (base, coxLet, coxIn)

relmapSplit :: (C.CList c, C.CRel c, B.Pretty c, C.CBool c)
  => C.RopUse c -> ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.Relmap c
relmapSplit use = C.relmapFlow use . relkitSplit

relkitSplit :: forall c. (C.CList c, C.CRel c, B.Pretty c, C.CBool c)
  => ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.RelkitCalc c
relkitSplit _ Nothing = Right C.relkitNothing
relkitSplit (base, deriv, bodies) (Just he1)
    | null ind  = Right kit2
    | otherwise = Message.unexpTermName
    where
      (ns, xs)    = unzip bodies            -- names and expressions
      ns1         = B.headNames he1         -- term names
      ind         = ns `B.snipIndex` ns1    -- shared indicies

      he2         = B.Relhead $ map term ns
      term n      = B.TermNest n $ B.headTerms he1
      kit2        = C.relkitJust he2 $ C.RelkitAbFull False kitf2 []
      kitf2 _ bo1 = do xs2 <- C.coxBeta base deriv he1 `mapM` xs
                       cs2 <- split xs2 bo1
                       Right [cs2]

      split :: [C.Cox c] -> [[c]] -> B.Ab [c]
      split [] _ = Right []
      split (x : xs2) bo1 =
          do bo2 <- run x `mapM` bo1
             let first = filter fst bo2
                 rest = map snd $ filter (not . fst) bo2
             rest' <- split xs2 rest
             let rel = B.Rel he1 $ map snd first
             Right $ C.pRel rel : rest'

      run :: C.Cox c -> [c] -> B.Ab (Bool, [c])
      run x cs = do c <- C.coxRun cs x
                    case C.isBool c of
                      True  -> Right (C.gBool c, cs)
                      False -> Message.reqBool
                   

-- ----------------------  alpha

alpha :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab (C.Cox c)
alpha = C.coxAlpha . C.globalSyntax . C.ropGlobal

alphas :: (C.CContent c) => C.RopUse c -> [B.Named B.TokenTree] -> B.Ab [C.NamedCox c]
alphas use = mapM (B.namedMapM $ alpha use)


{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koshucode.Baala.Op.Cox
( ropsCox,
  getContent,
  getOptContent,
  getContents,
  getFiller,

  -- * add
  consAdd, relmapAdd,

  -- * subst
  consSubst, relmapSubst,

  -- * filter
  consFilter, relmapFilter, relkitFilter,

  -- * omit-all
  consOmitAll, relmapOmitAll,

  -- * range
  consRange, relmapRange,
  -- $range

  -- * split
  consSplit, relmapSplit, relkitSplit,

  -- * unary
  consUnary, relmapUnary,
) where

import Prelude hiding (getContents)
import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Builtin  as Op
import qualified Koshucode.Baala.Op.Message  as Message


-- | Implementation of relational operators.
--
--   [@add \/N E ...@]
--     Add terms of name @\/N@ and content @E@ ...
--
--   [@keep E@]
--     Keep tuples @E@ equals true.
-- 
--   [@omit E@]
--     Omit tuples @E@ equals true.
-- 
ropsCox :: (C.CContent c) => [C.Rop c]
ropsCox = Op.ropList "cox"
    --         CONSTRUCTOR         USAGE                     ATTRIBUTE
    [ Op.ropV  consAdd             "add /N E ..."            "-in | -let"
    , Op.ropV  (consFilter True)   "keep E"                  "-in | -let"
    , Op.ropV  (consFilter False)  "omit E"                  "-in | -let"
    , Op.ropN  consOmitAll         "omit-all"                ""
    , Op.ropI  consRange           "range /N -from E -to E"  "-term | -from -to"
    , Op.ropV  consSplit           "split /N E ..."          "-in | -let"
    , Op.ropV  consSubst           "subst /N E ..."          "-in | -let"
    , Op.ropIV consUnary           "unary /N E ..."          "-term -expr"
    ]


-- ----------------------  alpha

ropAlpha :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab (C.Cox c)
ropAlpha = C.coxAlpha . C.globalSyntax . C.ropGlobal

ropNamedAlphas :: (C.CContent c) => C.RopUse c -> [B.Named B.TokenTree] -> B.Ab [C.NamedCox c]
ropNamedAlphas use = mapM (B.namedMapM $ ropAlpha use)

runCoxTree :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab c
runCoxTree use tree =
    do let base = C.globalFunction $ C.ropGlobal use
       alpha <- ropAlpha use tree
       beta  <- C.coxBeta base [] B.mempty alpha
       C.coxRun [] beta

getContents :: (C.CContent c) => C.RopUse c -> String -> B.Ab [c]
getContents use name =
    do trees <- Op.getTrees use name
       let trees2 = B.treeWrap `map` B.divideTreesByColon trees
       runCoxTree use `mapM` trees2

getContent :: (C.CContent c) => C.RopUse c -> String -> B.Ab c
getContent use name =
    do tree <- Op.getTree use name
       runCoxTree use tree

getOptContent :: (C.CContent c) => c -> C.RopUse c -> String -> B.Ab c
getOptContent opt = Op.getOption opt getContent

getFiller :: (C.CContent c) => C.RopUse c -> String -> B.Ab c
getFiller = getOptContent C.empty


-- ----------------------  add

consAdd :: (C.CContent c) => C.RopCons c
consAdd use =
    do treesLet  <-  Op.getOption [] Op.getWordTrees use "-let"
       treesIn   <-  Op.getTermTrees use "-in"
       coxLet    <-  ropNamedAlphas use treesLet
       coxIn     <-  ropNamedAlphas use treesIn
       let base = C.globalFunction $ C.ropGlobal use
       Right $ relmapAdd use (base, coxLet, coxIn)

relmapAdd :: (C.CList c, C.CRel c, B.Write c)
  => C.RopUse c -> ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.Relmap c
relmapAdd use = C.relmapFlow use . relkitAdd

relkitAdd :: (C.CList c, C.CRel c, B.Write c)
  => ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.RelkitFlow c
relkitAdd _ Nothing = Right C.relkitNothing
relkitAdd (base, deriv, bodies) (Just he1)
    | null ind  = Right kit2
    | otherwise = Message.unexpTermName
    where
      (ns, xs)    =  unzip bodies            -- names and expressions
      ns1         =  B.headNames he1         -- term names of input relation
      ind         =  ns `B.snipIndex` ns1    -- indicies for ns on input relation
      he2         =  ns `B.headAppend` he1
      kit2        =  C.relkitJust he2 $ C.RelkitOneToAbOne False kitf2 []
      kitf2 _ cs1 =  do xs2 <- C.coxBeta base deriv he1 `mapM` xs
                        cs2 <- C.coxRun cs1 `mapM` xs2
                        Right $ cs2 ++ cs1


-- ----------------------  subst

consSubst :: (C.CContent c) => C.RopCons c
consSubst use =
    do treesLet <- Op.getOption [] Op.getWordTrees use "-let"
       treesIn  <- Op.getTermTrees use "-in"
       coxLet   <- ropNamedAlphas use treesLet
       coxIn    <- ropNamedAlphas use treesIn
       let base = C.globalFunction $ C.ropGlobal use
       Right $ relmapSubst use (base, coxLet, coxIn)

relmapSubst :: (C.CList c, C.CRel c, B.Write c)
  => C.RopUse c -> ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.Relmap c
relmapSubst use = C.relmapFlow use . relkitSubst

relkitSubst :: (C.CList c, C.CRel c, B.Write c)
  => ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.RelkitFlow c
relkitSubst _ Nothing = Right C.relkitNothing
relkitSubst (base, deriv, bodies) (Just he1)
    | B.sameLength ns ind = Right kit2
    | otherwise           = Message.unexpTermName
    where
      (ns, xs)    =  unzip bodies               -- names and expressions
      ns1         =  B.headNames he1            -- term names of input relation
      ind         =  ns `B.snipIndex` ns1       -- indicies for ns on input relation
      cut         =  B.snipOff  ind             -- cutting-ns function
      fore        =  B.snipFore ind             -- cutting-ns function
      he2         =  B.headChange fore he1      -- heading of output relation
      kit2        =  C.relkitJust he2 $ C.RelkitOneToAbOne True kitf2 []
      kitf2 _ cs1 =  do xs2 <- C.coxBeta base deriv he1 `mapM` xs
                        cs2 <- C.coxRun cs1 `mapM` xs2
                        Right $ cs2 ++ cut cs1


-- ----------------------  filter

consFilter :: (C.CContent c) => Bool -> C.RopCons c
consFilter b use =
    do treesLet  <-  Op.getOption [] Op.getWordTrees use "-let"
       treesIn   <-  Op.getTrees use "-in"
       coxLet    <-  ropNamedAlphas use treesLet
       coxIn     <-  ropAlpha use $ B.treeWrap treesIn
       let base = C.globalFunction $ C.ropGlobal use
       Right $ relmapFilter use (b, base, coxLet, coxIn)

relmapFilter :: (C.CList c, C.CRel c, C.CBool c, B.Write c)
  => C.RopUse c -> (Bool, [C.Cop c], [C.NamedCox c], C.Cox c) -> C.Relmap c
relmapFilter use = C.relmapFlow use . relkitFilter

relkitFilter :: (C.CList c, C.CRel c, C.CBool c, B.Write c)
  => (Bool, [C.Cop c], [C.NamedCox c], C.Cox c) -> C.RelkitFlow c
relkitFilter _ Nothing = Right C.relkitNothing
relkitFilter (which, base, deriv, body) (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitAbPred p
    p cs1 = do e <- C.coxBeta base deriv he1 body
               c <- C.coxRun cs1 e
               case C.isBool c of
                 True  -> Right $ C.gBool c == which
                 False -> Message.reqBool


-- ----------------------  omit-all

consOmitAll :: C.RopCons c
consOmitAll use = Right $ relmapOmitAll use

relmapOmitAll :: C.RopUse c -> C.Relmap c
relmapOmitAll use = C.relmapFlow use relkitOmitAll

-- | Throw away all tuples in a relation.
relkitOmitAll :: C.RelkitFlow c
relkitOmitAll he1 = Right $ C.relkit he1 $ C.RelkitConst []


-- ----------------------  range

-- $range
--
--  Add term @\/n@ @0@, @\/n@ @1@, ..., and @\/n@ @9@.
--  
--    > range /n -from 0 -to 9

consRange :: (C.CContent c) => C.RopCons c
consRange use =
  do term      <-  Op.getTerm  use "-term"
     treeLow   <-  Op.getTrees use "-from"
     treeHigh  <-  Op.getTrees use "-to"
     coxLow    <-  ropAlpha use $ B.treeWrap treeLow
     coxHigh   <-  ropAlpha use $ B.treeWrap treeHigh
     let base = C.globalFunction $ C.ropGlobal use
     Right $ relmapRange use (term, base, coxLow, coxHigh)

relmapRange :: (C.CContent c) => C.RopUse c -> (B.TermName, [C.Cop c], C.Cox c, C.Cox c) -> C.Relmap c
relmapRange use = C.relmapFlow use . relkitRange

relkitRange :: (C.CContent c) => (B.TermName, [C.Cop c], C.Cox c, C.Cox c) -> C.RelkitFlow c
relkitRange _ Nothing = Right C.relkitNothing
relkitRange (n, base, coxLow, coxHigh) (Just he1) = Right kit2 where
    he2      = B.headCons n he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToAbMany False f2 []
    f2 _ cs  = do coxLow'   <-  C.coxBeta base [] he1 coxLow
                  coxHigh'  <-  C.coxBeta base [] he1 coxHigh
                  decLow    <-  C.getDec $ C.coxRun cs coxLow'
                  decHigh   <-  C.getDec $ C.coxRun cs coxHigh'

                  let low   =   B.decimalNum decLow
                      high  =   B.decimalNum decHigh
                      decs  =   map C.pDecFromInt [low .. high]

                  Right $ map (: cs) decs


-- ----------------------  split

consSplit :: (C.CContent c) => C.RopCons c
consSplit use =
    do treesLet  <-  Op.getOption [] Op.getWordTrees use "-let"
       treesIn   <-  Op.getTermTrees use "-in"
       coxLet    <-  ropNamedAlphas use treesLet
       coxIn     <-  ropNamedAlphas use treesIn
       let base = C.globalFunction $ C.ropGlobal use
       Right $ relmapSplit use (base, coxLet, coxIn)

relmapSplit :: (C.CList c, C.CRel c, B.Write c, C.CBool c)
  => C.RopUse c -> ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.Relmap c
relmapSplit use = C.relmapFlow use . relkitSplit

relkitSplit :: forall c. (C.CList c, C.CRel c, B.Write c, C.CBool c)
  => ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.RelkitFlow c
relkitSplit _ Nothing = Right C.relkitNothing
relkitSplit (base, deriv, bodies) (Just he1)
    | null ind  = Right kit2
    | otherwise = Message.unexpTermName
    where
      (ns, xs)    =  unzip bodies            -- names and expressions
      ns1         =  B.headNames he1         -- term names
      ind         =  ns `B.snipIndex` ns1    -- shared indicies

      he2         =  B.Relhead $ map term ns
      term n      =  B.TermNest n $ B.headTerms he1
      kit2        =  C.relkitJust he2 $ C.RelkitAbFull False kitf2 []
      kitf2 _ bo1 =  do xs2 <- C.coxBeta base deriv he1 `mapM` xs
                        cs2 <- split xs2 bo1
                        Right [cs2]

      split :: [C.Cox c] -> [[c]] -> B.Ab [c]
      split [] _ = Right []
      split (x : xs2) bo1 =
          do bo2 <- run x `mapM` bo1
             let first = filter fst bo2
                 rest  = map snd $ B.omit fst bo2
             rest' <- split xs2 rest
             let rel = B.Rel he1 $ map snd first
             Right $ C.pRel rel : rest'

      run :: C.Cox c -> [c] -> B.Ab (Bool, [c])
      run x cs = do c <- C.coxRun cs x
                    case C.isBool c of
                      True  -> Right (C.gBool c, cs)
                      False -> Message.reqBool


-- ----------------------  unary

--  > unary /n 1 : 2 : 3

consUnary :: (C.CContent c) => C.RopCons c
consUnary use =
    do n  <- Op.getTerm  use "-term"
       cs <- getContents use "-expr"
       Right $ relmapUnary use (n, cs)

relmapUnary :: (C.CContent c) => C.RopUse c -> (B.TermName, [c]) -> C.Relmap c
relmapUnary use = C.relmapFlow use . relkitUnary

relkitUnary :: (C.CContent c) => (B.TermName, [c]) -> C.RelkitFlow c
relkitUnary (n, cs) _ = Right kit2 where
    he2    = B.headFrom [n]
    kit2   = C.relkitJust he2 $ C.RelkitAbFull True f2 []
    f2 _ _ = Right $ map B.li1 cs


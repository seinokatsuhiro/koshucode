{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koshucode.Baala.Op.Cox
( ropsCox,
  getCox,
  getContent,
  getOptContent,
  getContents,
  getFiller,

  -- * add
  consAdd, relmapAdd,

  -- * subst
  consSubst, relmapSubst,

  -- * keep & omit
  consFilter, relmapFilter, relkitFilter,

  -- * contain
  consContain, relmapContain, relkitContain,

  -- * omit-all
  consOmitAll, relmapOmitAll,

  -- * range
  consRange, relmapRange,
  -- $range

  -- * fill
  consFill, relmapFill,

  -- * replace
  consReplace,

  -- * replace-all
  consReplaceAll, relmapReplaceAll,

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
    --          CONSTRUCTOR         USAGE                        ATTRIBUTE
    [ Op.ropV   consAdd             "add /N E ..."               "-in | -let"
    , Op.ropI   consContain         "contain E"                  "-expr"
    , Op.ropV   (consFilter True)   "keep E"                     "-in | -let"
    , Op.ropV   (consFilter False)  "omit E"                     "-in | -let"
    , Op.ropN   consOmitAll         "omit-all"                   ""
    , Op.ropI   consRange           "range /N -from E -to E"     "-term | -from -to"
    , Op.ropTI  consFill            "fill /P E"                  "-term -to"
    , Op.ropTI  consReplace         "replace /P E"               "-term -by"
    , Op.ropN   consReplaceAll      "replace-all -from E -to E"  "| -from -to"
    , Op.ropV   consSplit           "split /N E ..."             "-in | -let"
    , Op.ropV   consSubst           "subst /N E ..."             "-in | -let"
    , Op.ropIV  consUnary           "unary /N E ..."             "-term -expr"
    ]


-- ----------------------  alpha

ropBase :: C.RopUse c -> [C.Cop c]
ropBase = C.globalFunction . C.ropGlobal

ropAlpha :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab (C.Cox c)
ropAlpha = C.coxAlpha . C.globalSyntax . C.ropGlobal

ropNamedAlphas :: (C.CContent c) => C.RopUse c -> [B.Named B.TokenTree] -> B.Ab [C.NamedCox c]
ropNamedAlphas use = mapM (B.namedMapM $ ropAlpha use)

getCox :: (C.CContent c) => C.RopUse c -> String -> B.Ab (C.Cox c)
getCox use = ropAlpha use . B.treeWrap B.<=< Op.getTrees use

getNamedCoxes :: (C.CContent c) => C.RopUse c -> String -> B.Ab [C.NamedCox c]
getNamedCoxes use = ropNamedAlphas use B.<=< Op.getWordTrees use 

getTermCoxes :: (C.CContent c) => C.RopUse c -> String -> B.Ab [C.NamedCox c]
getTermCoxes use = ropNamedAlphas use B.<=< Op.getTermTrees use

getContents :: (C.CContent c) => C.RopUse c -> String -> B.Ab [c]
getContents use name =
    do trees <- Op.getTrees use name
       let trees2 = B.treeWrap `map` B.divideTreesByColon trees
       runCoxTree use `mapM` trees2

getContent :: (C.CContent c) => C.RopUse c -> String -> B.Ab c
getContent use name =
    do tree <- Op.getTree use name
       runCoxTree use tree

runCoxTree :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab c
runCoxTree use tree =
    do alpha <- ropAlpha use tree
       C.coxRunCox (ropBase use, []) B.mempty [] alpha

getOptContent :: (C.CContent c) => c -> C.RopUse c -> String -> B.Ab c
getOptContent opt = Op.getOption opt getContent

getFiller :: (C.CContent c) => C.RopUse c -> String -> B.Ab c
getFiller = getOptContent C.empty


-- ----------------------  add

consAdd :: (C.CContent c) => C.RopCons c
consAdd use =
    do coxLet <- Op.getOption [] getNamedCoxes use "-let"
       coxIn  <- getTermCoxes use "-in"
       Right $ relmapAdd use ((ropBase use, coxLet), coxIn)

relmapAdd :: (C.CList c, C.CRel c, B.Write c)
  => C.RopUse c -> (C.CopBundle c, [C.NamedCox c]) -> C.Relmap c
relmapAdd use = C.relmapFlow use . relkitAdd

relkitAdd :: (C.CList c, C.CRel c, B.Write c)
  => (C.CopBundle c, [C.NamedCox c]) -> C.RelkitFlow c
relkitAdd _ Nothing = Right C.relkitNothing
relkitAdd (cops, bodies) (Just he1)
    | null ind  = Right kit2
    | otherwise = Message.unexpTermName
    where
      (ns, xs)    =  unzip bodies            -- names and expressions
      ns1         =  B.headNames he1         -- term names of input relation
      ind         =  ns `B.snipIndex` ns1    -- indicies for ns on input relation
      he2         =  ns `B.headAppend` he1
      kit2        =  C.relkitJust he2 $ C.RelkitOneToAbOne False kitf2 []
      kitf2 _ cs1 =  do cs2 <- C.coxRunCox cops he1 cs1 `mapM` xs
                        Right $ cs2 ++ cs1


-- ----------------------  subst

consSubst :: (C.CContent c) => C.RopCons c
consSubst use =
    do coxLet <- Op.getOption [] getNamedCoxes use "-let"
       coxIn  <- getTermCoxes use "-in"
       Right $ relmapSubst use ((ropBase use, coxLet), coxIn)

relmapSubst :: (C.CList c, C.CRel c, B.Write c)
  => C.RopUse c -> (C.CopBundle c, [C.NamedCox c]) -> C.Relmap c
relmapSubst use = C.relmapFlow use . relkitSubst

relkitSubst :: (C.CList c, C.CRel c, B.Write c)
  => (C.CopBundle c, [C.NamedCox c]) -> C.RelkitFlow c
relkitSubst _ Nothing = Right C.relkitNothing
relkitSubst (cops, bodies) (Just he1)
    | B.sameLength ns ind = Right kit2
    | otherwise           = Message.unexpTermName
    where
      (ns, xs)  =  unzip bodies               -- names and expressions
      ns1       =  B.headNames he1            -- term names of input relation
      ind       =  ns `B.snipIndex` ns1       -- indicies for ns on input relation
      cut       =  B.snipOff  ind             -- cutting-ns function
      fore      =  B.snipFore ind             -- cutting-ns function
      he2       =  B.headChange fore he1      -- heading of output relation
      kit2      =  C.relkitJust he2 $ C.RelkitOneToAbOne True f2 []
      f2 _ cs1  =  do cs2 <- C.coxRunCox cops he1 cs1 `mapM` xs
                      Right $ cs2 ++ cut cs1


-- ----------------------  filter

consFilter :: (C.CContent c) => Bool -> C.RopCons c
consFilter b use =
    do coxLet  <- Op.getOption [] getNamedCoxes use "-let"
       coxIn   <- getCox use "-in"
       Right $ relmapFilter use (b, (ropBase use, coxLet), coxIn)

relmapFilter :: (C.CList c, C.CRel c, C.CBool c, B.Write c)
  => C.RopUse c -> (Bool, C.CopBundle c, C.Cox c) -> C.Relmap c
relmapFilter use = C.relmapFlow use . relkitFilter

relkitFilter :: (C.CList c, C.CRel c, C.CBool c, B.Write c)
  => (Bool, C.CopBundle c, C.Cox c) -> C.RelkitFlow c
relkitFilter _ Nothing = Right C.relkitNothing
relkitFilter (which, cops, body) (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitAbPred p
    p cs1 = do c <- C.coxRunCox cops he1 cs1 body
               case C.isBool c of
                 True  -> Right $ C.gBool c == which
                 False -> Message.reqBool


-- ----------------------  contain

consContain :: (C.CContent c) => C.RopCons c
consContain use =
    do c <- getContent use "-expr"
       Right $ relmapContain use c

relmapContain :: (Eq c) => C.RopUse c -> c -> C.Relmap c
relmapContain use = C.relmapFlow use . relkitContain

relkitContain :: (Eq c) => c -> C.RelkitFlow c
relkitContain _ Nothing = Right C.relkitNothing
relkitContain c (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitAbPred p
    p cs1 = Right $ c `elem` cs1


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
  do term     <- Op.getTerm use "-term"
     coxLow   <- getCox use "-from"
     coxHigh  <- getCox use "-to"
     Right $ relmapRange use (term, ropBase use, coxLow, coxHigh)

relmapRange :: (C.CContent c) => C.RopUse c -> (B.TermName, [C.Cop c], C.Cox c, C.Cox c) -> C.Relmap c
relmapRange use = C.relmapFlow use . relkitRange

relkitRange :: (C.CContent c) => (B.TermName, [C.Cop c], C.Cox c, C.Cox c) -> C.RelkitFlow c
relkitRange _ Nothing = Right C.relkitNothing
relkitRange (n, base, coxLow, coxHigh) (Just he1) = Right kit2 where
    he2      = B.headCons n he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToAbMany False f2 []
    f2 _ cs  = do decLow    <-  C.getDec $ C.coxRunCox (base, []) he1 cs coxLow
                  decHigh   <-  C.getDec $ C.coxRunCox (base, []) he1 cs coxHigh

                  let low   =   B.decimalNum decLow
                      high  =   B.decimalNum decHigh
                      decs  =   map C.pDecFromInt [low .. high]

                  Right $ map (: cs) decs


-- ----------------------  fill

--    > fill /a /b 0

consFill :: (C.CContent c) => C.RopCons c
consFill use =
  do ns    <- Op.getTerms use "-term"
     coxTo <- getCox use "-to"
     Right $ relmapFill use (ns, ropBase use, coxTo)

relmapFill :: (C.CContent c) => C.RopUse c -> ([B.TermName], [C.Cop c], C.Cox c) -> C.Relmap c
relmapFill use = C.relmapFlow use . relkitFill

relkitFill :: (C.CContent c) => ([B.TermName], [C.Cop c], C.Cox c) -> C.RelkitFlow c
relkitFill _ Nothing = Right C.relkitNothing
relkitFill (ns, base, coxTo) (Just he1) = Right kit2 where
    ns1       =  B.headNames he1
    ind       =  ns `B.snipIndex` ns1
    pick      =  B.snipFrom ind
    cut       =  B.snipOff  ind
    fore      =  B.snipFore ind
    he2       =  B.headChange fore he1
    kit2      =  C.relkitJust he2 $ C.RelkitOneToAbOne True f2 []
    f2 _ cs1  =  do cTo  <- C.coxRunCox (base, []) he1 cs1 coxTo
                    let fill c | C.isEmpty c = cTo
                               | otherwise   = c
                    Right $ map fill (pick cs1) ++ (cut cs1)


-- ----------------------  replace

--    > replace /a /b (| x | if x < 0 -> 0 : x |)

consReplace :: (C.CContent c) => C.RopCons c
consReplace use =
    do ns     <- Op.getTerms use "-term"
       coxBy  <- getCox use "-by"
       B.unless (C.coxSyntacticArity coxBy == 1) $ do
         B.abortable "relmap-replace" [coxBy] Message.reqUnaryFn
       let expr n = (n, C.CoxApplyL [] coxBy [C.CoxTerm [] [n] []])
       Right $ relmapSubst use ((ropBase use, []), map expr ns)


-- ----------------------  replace-all

--    > replace-all -from () -to 0

consReplaceAll :: (C.CContent c) => C.RopCons c
consReplaceAll use =
  do coxFrom <- getCox use "-from"
     coxTo   <- getCox use "-to"
     Right $ relmapReplaceAll use (ropBase use, coxFrom, coxTo)

relmapReplaceAll :: (C.CContent c) => C.RopUse c -> ([C.Cop c], C.Cox c, C.Cox c) -> C.Relmap c
relmapReplaceAll use = C.relmapFlow use . relkitReplaceAll

relkitReplaceAll :: (C.CContent c) => ([C.Cop c], C.Cox c, C.Cox c) -> C.RelkitFlow c
relkitReplaceAll _ Nothing = Right C.relkitNothing
relkitReplaceAll (base, coxFrom, coxTo) (Just he1) = Right kit2 where
    kit2     = C.relkitJust he1 $ C.RelkitOneToAbOne False f2 []
    f2 _ cs  = do cFrom    <-  C.coxRunCox (base, []) he1 cs coxFrom
                  cTo      <-  C.coxRunCox (base, []) he1 cs coxTo
                  let replace c | c == cFrom = cTo
                                | otherwise  = c
                  Right $ map replace cs


-- ----------------------  split

consSplit :: (C.CContent c) => C.RopCons c
consSplit use =
    do coxLet <- Op.getOption [] getNamedCoxes use "-let"
       coxIn  <- getTermCoxes use "-in"
       Right $ relmapSplit use ((ropBase use, coxLet), coxIn)

relmapSplit :: (C.CList c, C.CRel c, B.Write c, C.CBool c)
  => C.RopUse c -> (C.CopBundle c, [C.NamedCox c]) -> C.Relmap c
relmapSplit use = C.relmapFlow use . relkitSplit

relkitSplit :: forall c. (C.CList c, C.CRel c, B.Write c, C.CBool c)
  => (C.CopBundle c, [C.NamedCox c]) -> C.RelkitFlow c
relkitSplit _ Nothing = Right C.relkitNothing
relkitSplit (cops, bodies) (Just he1)
    | null ind  = Right kit2
    | otherwise = Message.unexpTermName
    where
      (ns, xs)    =  unzip bodies            -- names and expressions
      ns1         =  B.headNames he1         -- term names
      ind         =  ns `B.snipIndex` ns1    -- shared indicies

      he2         =  B.Relhead $ map term ns
      term n      =  B.TermNest n $ B.headTerms he1
      kit2        =  C.relkitJust he2 $ C.RelkitAbFull False kitf2 []
      kitf2 _ bo1 =  do let fs2 = C.coxRunList cops he1 `map` xs
                        cs2 <- split fs2 bo1
                        Right [cs2]

      split :: [C.RunList c] -> [[c]] -> B.Ab [c]
      split [] _ = Right []
      split (f : fs2) bo1 =
          do bo2 <- run f `mapM` bo1
             let first = filter fst bo2
                 rest  = map snd $ B.omit fst bo2
             rest' <- split fs2 rest
             let rel = B.Rel he1 $ map snd first
             Right $ C.pRel rel : rest'

      run :: C.RunList c -> [c] -> B.Ab (Bool, [c])
      run f cs = do c <- f cs
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


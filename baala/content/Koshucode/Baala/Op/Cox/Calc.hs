{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koshucode.Baala.Op.Cox.Calc
  ( ropsCoxCalc,
  
    -- * add
    consAdd, relmapAdd,
  
    -- * subst
    consSubst, relmapSubst,
  
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
import qualified Koshucode.Baala.Op.Cox.Get  as Op
import qualified Koshucode.Baala.Op.Message  as Msg


-- | Implementation of relational operators.
--
--   [@add \/N E ...@]
--     Add terms of name @\/N@ and content @E@ ...
-- 
ropsCoxCalc :: (C.CContent c) => [C.Rop c]
ropsCoxCalc = Op.ropList "cox-calc"
    --        CONSTRUCTOR       USAGE                          ATTRIBUTE
    [ Op.def  consAdd           "add /N E ..."                 "V -cox | -where"
    , Op.def  consFill          "fill /P E"                    "T1 -term -to"
    , Op.def  consReplace       "replace /P E"                 "T1 -term -by"
    , Op.def  consReplaceAll    "replace-all -from E -to E"    "0 | -from -to"
    , Op.def  consSplit         "split /N E ..."               "V -cox | -where"
    , Op.def  consSubst         "subst /N E ..."               "V -cox | -where"
    , Op.def  consUnary         "unary /N E ..."               "1V -term -expr"
    , Op.def  consDumpCox       "dump-cox E"                   "V -cox"
    ]


-- ----------------------  add

consAdd :: (C.CContent c) => C.RopCons c
consAdd use =
    do cops <- Op.getWhere use "-where"
       cox <- Op.getTermCoxes use "-cox"
       Right $ relmapAdd use (cops, cox)

relmapAdd :: (C.CList c, C.CRel c, B.Write c)
  => C.RopUse c -> (C.CopSet c, [C.NamedCox c]) -> C.Relmap c
relmapAdd use = C.relmapFlow use . relkitAdd

relkitAdd :: (C.CList c, C.CRel c, B.Write c)
  => (C.CopSet c, [C.NamedCox c]) -> C.RelkitFlow c
relkitAdd _ Nothing = Right C.relkitNothing
relkitAdd (cops, cox) (Just he1)
    | null ind  = Right kit2
    | otherwise = Msg.reqNewTerm ns he1
    where
      (ns, xs)    =  unzip cox               -- names and expressions
      ns1         =  B.headNames he1         -- term names of input relation
      ind         =  ns `B.snipIndex` ns1    -- indicies for ns on input relation
      he2         =  ns `B.headAppend` he1
      kit2        =  C.relkitJust he2 $ C.RelkitOneToAbOne False kitf2 []
      kitf2 _ cs1 =  do cs2 <- C.coxRunCox cops he1 cs1 `mapM` xs
                        Right $ cs2 ++ cs1


-- ----------------------  subst

consSubst :: (C.CContent c) => C.RopCons c
consSubst use =
    do cops <- Op.getWhere use "-where"
       cox <- Op.getTermCoxes use "-cox"
       Right $ relmapSubst use (cops, cox)

relmapSubst :: (C.CList c, C.CRel c, B.Write c)
  => C.RopUse c -> (C.CopSet c, [C.NamedCox c]) -> C.Relmap c
relmapSubst use = C.relmapFlow use . relkitSubst

relkitSubst :: (C.CList c, C.CRel c, B.Write c)
  => (C.CopSet c, [C.NamedCox c]) -> C.RelkitFlow c
relkitSubst _ Nothing = Right C.relkitNothing
relkitSubst (cops, cox) (Just he1)
    | B.sameLength ns ind = Right kit2
    | otherwise           = Msg.unexpTermName
    where
      (ns, xs)  =  unzip cox                  -- names and expressions
      ns1       =  B.headNames he1            -- term names of input relation
      ind       =  ns `B.snipIndex` ns1       -- indicies for ns on input relation
      cut       =  B.snipOff  ind             -- cutting-ns function
      fore      =  B.snipFore ind             -- cutting-ns function
      he2       =  B.headMap fore he1      -- heading of output relation
      kit2      =  C.relkitJust he2 $ C.RelkitOneToAbOne True f2 []
      f2 _ cs1  =  do cs2 <- C.coxRunCox cops he1 cs1 `mapM` xs
                      Right $ cs2 ++ cut cs1


-- ----------------------  fill

--    > fill /a /b 0

consFill :: (C.CContent c) => C.RopCons c
consFill use =
  do ns    <- Op.getTerms use "-term"
     coxTo <- Op.getCox use "-to"
     let cops = C.globalCopset $ C.ropGlobal use
     Right $ relmapFill use (ns, cops, coxTo)

relmapFill :: (C.CContent c) => C.RopUse c -> ([B.TermName], C.CopSet c, C.Cox c) -> C.Relmap c
relmapFill use = C.relmapFlow use . relkitFill

relkitFill :: (C.CContent c) => ([B.TermName], C.CopSet c, C.Cox c) -> C.RelkitFlow c
relkitFill _ Nothing = Right C.relkitNothing
relkitFill (ns, cops, coxTo) (Just he1) = Right kit2 where
    ns1       =  B.headNames he1
    ind       =  ns `B.snipIndex` ns1
    pick      =  B.snipFrom ind
    cut       =  B.snipOff  ind
    fore      =  B.snipFore ind
    he2       =  B.headMap fore he1
    kit2      =  C.relkitJust he2 $ C.RelkitOneToAbOne True f2 []
    f2 _ cs1  =  do cTo  <- C.coxRunCox cops he1 cs1 coxTo
                    let fill c | C.isEmpty c = cTo
                               | otherwise   = c
                    Right $ map fill (pick cs1) ++ (cut cs1)


-- ----------------------  replace

--    > replace /a /b (| x | if x < 0 -> 0 : x |)

consReplace :: (C.CContent c) => C.RopCons c
consReplace use =
    do ns     <- Op.getTerms use "-term"
       coxBy  <- Op.getCox use "-by"
       B.unless (C.coxSyntacticArity coxBy == 1) $ do
         B.abortable "relmap-replace" [coxBy] Msg.reqUnaryFn
       let expr n = (n, C.CoxFill [] coxBy [C.CoxTerm [] [n] []])
           cops   = C.globalCopset $ C.ropGlobal use
       Right $ relmapSubst use (cops, map expr ns)


-- ----------------------  replace-all

--    > replace-all -from () -to 0

consReplaceAll :: (C.CContent c) => C.RopCons c
consReplaceAll use =
  do coxFrom <- Op.getCox use "-from"
     coxTo   <- Op.getCox use "-to"
     let cops = C.globalCopset $ C.ropGlobal use
     Right $ relmapReplaceAll use (cops, coxFrom, coxTo)

relmapReplaceAll :: (C.CContent c) => C.RopUse c -> (C.CopSet c, C.Cox c, C.Cox c) -> C.Relmap c
relmapReplaceAll use = C.relmapFlow use . relkitReplaceAll

relkitReplaceAll :: (C.CContent c) => (C.CopSet c, C.Cox c, C.Cox c) -> C.RelkitFlow c
relkitReplaceAll _ Nothing = Right C.relkitNothing
relkitReplaceAll (cops, coxFrom, coxTo) (Just he1) = Right kit2 where
    kit2     = C.relkitJust he1 $ C.RelkitOneToAbOne False f2 []
    f2 _ cs  = do cFrom    <-  C.coxRunCox cops he1 cs coxFrom
                  cTo      <-  C.coxRunCox cops he1 cs coxTo
                  let replace c | c == cFrom = cTo
                                | otherwise  = c
                  Right $ map replace cs


-- ----------------------  split

consSplit :: (C.CContent c) => C.RopCons c
consSplit use =
    do cops <- Op.getWhere use "-where"
       cox <- Op.getTermCoxes use "-cox"
       Right $ relmapSplit use (cops, cox)

relmapSplit :: (C.CList c, C.CRel c, B.Write c, C.CBool c)
  => C.RopUse c -> (C.CopSet c, [C.NamedCox c]) -> C.Relmap c
relmapSplit use = C.relmapFlow use . relkitSplit

relkitSplit :: forall c. (C.CList c, C.CRel c, B.Write c, C.CBool c)
  => (C.CopSet c, [C.NamedCox c]) -> C.RelkitFlow c
relkitSplit _ Nothing = Right C.relkitNothing
relkitSplit (cops, cox) (Just he1)
    | null ind  = Right kit2
    | otherwise = Msg.unexpTermName
    where
      (ns, xs)    =  unzip cox               -- names and expressions
      ns1         =  B.headNames he1         -- term names
      ind         =  ns `B.snipIndex` ns1    -- shared indicies

      he2         =  B.headNests ns he1
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
                      False -> Msg.reqBool


-- ----------------------  unary

--  > unary /n 1 : 2 : 3

consUnary :: (C.CContent c) => C.RopCons c
consUnary use =
    do n  <- Op.getTerm  use "-term"
       cs <- Op.getContents use "-expr"
       Right $ relmapUnary use (n, cs)

relmapUnary :: (C.CContent c) => C.RopUse c -> (B.TermName, [c]) -> C.Relmap c
relmapUnary use = C.relmapFlow use . relkitUnary

relkitUnary :: (C.CContent c) => (B.TermName, [c]) -> C.RelkitFlow c
relkitUnary (n, cs) _ = Right kit2 where
    he2    = B.headFrom [n]
    kit2   = C.relkitJust he2 $ C.RelkitAbFull True f2 []
    f2 _ _ = Right $ map B.li1 cs



-- ----------------------  dump-cox

--  > dump-cox /x >= 0

consDumpCox :: (C.CContent c) => C.RopCons c
consDumpCox use =
    do cox <- Op.getCox use "-cox"
       Msg.dumpCox cox


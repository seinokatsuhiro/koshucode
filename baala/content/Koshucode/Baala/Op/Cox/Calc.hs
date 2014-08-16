{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koshucode.Baala.Op.Cox.Calc
( ropsCoxCalc,

  -- * add
  consAdd, relmapAdd,

  -- * subst
  consSubst, relmapSubst,

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
import qualified Koshucode.Baala.Op.Cox.Get  as Op
import qualified Koshucode.Baala.Op.Message  as Message


-- | Implementation of relational operators.
--
--   [@add \/N E ...@]
--     Add terms of name @\/N@ and content @E@ ...
-- 
ropsCoxCalc :: (C.CContent c) => [C.Rop c]
ropsCoxCalc = Op.ropList "cox-calc"
    --          CONSTRUCTOR         USAGE                        ATTRIBUTE
    [ Op.ropV   consAdd             "add /N E ..."               "-cox | -where"
    , Op.ropI   consRange           "range /N -from E -to E"     "-term | -from -to"
    , Op.ropTI  consFill            "fill /P E"                  "-term -to"
    , Op.ropTI  consReplace         "replace /P E"               "-term -by"
    , Op.ropN   consReplaceAll      "replace-all -from E -to E"  "| -from -to"
    , Op.ropV   consSplit           "split /N E ..."             "-cox | -where"
    , Op.ropV   consSubst           "subst /N E ..."             "-cox | -where"
    , Op.ropIV  consUnary           "unary /N E ..."             "-term -expr"
    , Op.ropV   consDumpCox         "dump-cox E"                 "-cox"
    ]


-- ----------------------  add

consAdd :: (C.CContent c) => C.RopCons c
consAdd use =
    do def <- Op.getWhere use "-where"
       cox <- Op.getTermCoxes use "-cox"
       Right $ relmapAdd use (def, cox)

relmapAdd :: (C.CList c, C.CRel c, B.Write c)
  => C.RopUse c -> (C.CopBundle c, [C.NamedCox c]) -> C.Relmap c
relmapAdd use = C.relmapFlow use . relkitAdd

relkitAdd :: (C.CList c, C.CRel c, B.Write c)
  => (C.CopBundle c, [C.NamedCox c]) -> C.RelkitFlow c
relkitAdd _ Nothing = Right C.relkitNothing
relkitAdd (def, cox) (Just he1)
    | null ind  = Right kit2
    | otherwise = Message.unexpTermName
    where
      (ns, xs)    =  unzip cox               -- names and expressions
      ns1         =  B.headNames he1         -- term names of input relation
      ind         =  ns `B.snipIndex` ns1    -- indicies for ns on input relation
      he2         =  ns `B.headAppend` he1
      kit2        =  C.relkitJust he2 $ C.RelkitOneToAbOne False kitf2 []
      kitf2 _ cs1 =  do cs2 <- C.coxRunCox def he1 cs1 `mapM` xs
                        Right $ cs2 ++ cs1


-- ----------------------  subst

consSubst :: (C.CContent c) => C.RopCons c
consSubst use =
    do def <- Op.getWhere use "-where"
       cox <- Op.getTermCoxes use "-cox"
       Right $ relmapSubst use (def, cox)

relmapSubst :: (C.CList c, C.CRel c, B.Write c)
  => C.RopUse c -> (C.CopBundle c, [C.NamedCox c]) -> C.Relmap c
relmapSubst use = C.relmapFlow use . relkitSubst

relkitSubst :: (C.CList c, C.CRel c, B.Write c)
  => (C.CopBundle c, [C.NamedCox c]) -> C.RelkitFlow c
relkitSubst _ Nothing = Right C.relkitNothing
relkitSubst (def, cox) (Just he1)
    | B.sameLength ns ind = Right kit2
    | otherwise           = Message.unexpTermName
    where
      (ns, xs)  =  unzip cox                  -- names and expressions
      ns1       =  B.headNames he1            -- term names of input relation
      ind       =  ns `B.snipIndex` ns1       -- indicies for ns on input relation
      cut       =  B.snipOff  ind             -- cutting-ns function
      fore      =  B.snipFore ind             -- cutting-ns function
      he2       =  B.headChange fore he1      -- heading of output relation
      kit2      =  C.relkitJust he2 $ C.RelkitOneToAbOne True f2 []
      f2 _ cs1  =  do cs2 <- C.coxRunCox def he1 cs1 `mapM` xs
                      Right $ cs2 ++ cut cs1


-- ----------------------  range

-- $range
--
--  Add term @\/n@ @0@, @\/n@ @1@, ..., and @\/n@ @9@.
--  
--    > range /n -from 0 -to 9

consRange :: (C.CContent c) => C.RopCons c
consRange use =
  do term     <- Op.getTerm use "-term"
     coxLow   <- Op.getCox use "-from"
     coxHigh  <- Op.getCox use "-to"
     Right $ relmapRange use (term, Op.ropBase use, coxLow, coxHigh)

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
     coxTo <- Op.getCox use "-to"
     Right $ relmapFill use (ns, Op.ropBase use, coxTo)

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
       coxBy  <- Op.getCox use "-by"
       B.unless (C.coxSyntacticArity coxBy == 1) $ do
         B.abortable "relmap-replace" [coxBy] Message.reqUnaryFn
       let expr n = (n, C.CoxRefill [] coxBy [C.CoxTerm [] [n] []])
       Right $ relmapSubst use ((Op.ropBase use, []), map expr ns)


-- ----------------------  replace-all

--    > replace-all -from () -to 0

consReplaceAll :: (C.CContent c) => C.RopCons c
consReplaceAll use =
  do coxFrom <- Op.getCox use "-from"
     coxTo   <- Op.getCox use "-to"
     Right $ relmapReplaceAll use (Op.ropBase use, coxFrom, coxTo)

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
    do def <- Op.getWhere use "-where"
       cox <- Op.getTermCoxes use "-cox"
       Right $ relmapSplit use (def, cox)

relmapSplit :: (C.CList c, C.CRel c, B.Write c, C.CBool c)
  => C.RopUse c -> (C.CopBundle c, [C.NamedCox c]) -> C.Relmap c
relmapSplit use = C.relmapFlow use . relkitSplit

relkitSplit :: forall c. (C.CList c, C.CRel c, B.Write c, C.CBool c)
  => (C.CopBundle c, [C.NamedCox c]) -> C.RelkitFlow c
relkitSplit _ Nothing = Right C.relkitNothing
relkitSplit (def, cox) (Just he1)
    | null ind  = Right kit2
    | otherwise = Message.unexpTermName
    where
      (ns, xs)    =  unzip cox               -- names and expressions
      ns1         =  B.headNames he1         -- term names
      ind         =  ns `B.snipIndex` ns1    -- shared indicies

      he2         =  B.Relhead $ map term ns
      term n      =  B.TermNest n $ B.headTerms he1
      kit2        =  C.relkitJust he2 $ C.RelkitAbFull False kitf2 []
      kitf2 _ bo1 =  do let fs2 = C.coxRunList def he1 `map` xs
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
       Message.dumpCox cox


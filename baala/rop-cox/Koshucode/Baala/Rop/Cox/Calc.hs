{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Relmap operator with content calculation.

module Koshucode.Baala.Rop.Cox.Calc
  ( ropsCoxCalc,
    -- * add
    consAdd, relmapAdd,
    -- * alt
    consSubst, relmapSubst,
    -- * fill
    consFill, relmapFill,
    -- * map
    consReplace,
    -- * replace-all
    consReplaceAll, relmapReplaceAll,
    -- * split
    consSplit, relmapSplit, relkitSplit,
    -- * unary
    consUnary, relmapUnary,
  ) where

import Prelude hiding (getContents)
import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Syntax          as S
import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Rop.Base        as Op
import qualified Koshucode.Baala.Rop.Cox.Get     as Op
import qualified Koshucode.Baala.Rop.Cox.Message as Msg


-- | Implementation of relational operators.
--
--   [@add \/N E ...@]
--     Add terms of name @\/N@ and content @E@ ...
-- 
ropsCoxCalc :: (D.CContent c) => [C.Rop c]
ropsCoxCalc = Op.ropList "cox-calc"
    --        CONSTRUCTOR       USAGE                          ATTRIBUTE
    [ Op.def  consAdd           "add /N E ..."                 "-cox* . -where?"
    , Op.def  consFill          "fill /P ... -with E"          "-term* . -with"
    , Op.def  consReplace       "replace /P ... -by F"         "-term* . -by"
    , Op.def  consReplaceAll    "replace-all -from E -to E"    ". -from -to"
    , Op.def  consSplit         "split /N E ..."               "-cox* . -where?"
    , Op.def  consSubst         "subst /N E ..."               "-cox* . -where?"
    , Op.def  consUnary         "unary /N E ..."               "-term -expr*"
    , Op.def  consDumpCox       "dump-cox E"                   "-cox*"
    ]


-- ----------------------  add

-- | __add \/N E ...__
consAdd :: (D.CContent c) => C.RopCons c
consAdd med =
    do cops <- Op.getWhere med "-where"
       cox <- Op.getTermCoxes med "-cox"
       Right $ relmapAdd med (cops, cox)

-- | Create @add@ relmap.
relmapAdd :: (D.CContent c) => C.Intmed c -> (D.CopSet c, [D.NamedCox c]) -> C.Relmap c
relmapAdd med = C.relmapFlow med . relkitAdd

-- | Create @add@ relkit.
relkitAdd :: (D.CContent c) => (D.CopSet c, [D.NamedCox c]) -> C.RelkitFlow c
relkitAdd _ Nothing = Right C.relkitNothing
relkitAdd (cops, cox) (Just he1)
    | D.preTermsExist pk = Msg.reqNewTerm (D.ssLShareNames pk) he1
    | otherwise          = Right kit2
    where
      (ns, xs)    = unzip cox       -- names and expressions
      pk          = D.termPicker ns he1
      he2         = ns `D.headAppend` he1
      kit2        = C.relkitJust he2 $ C.RelkitOneToAbOne False kitf2 []
      kitf2 _ cs1 = do cs2 <- D.coxRunCox cops he1 cs1 `mapM` xs
                       Right $ cs2 ++ cs1


-- ----------------------  subst

-- | __alt \/P E ...__
consSubst :: (D.CContent c) => C.RopCons c
consSubst med =
    do cops <- Op.getWhere med "-where"
       cox <- Op.getTermCoxes med "-cox"
       Right $ relmapSubst med (cops, cox)

-- | Create @alt@ relmap.
relmapSubst :: (D.CContent c) => C.Intmed c -> (D.CopSet c, [D.NamedCox c]) -> C.Relmap c
relmapSubst med = C.relmapFlow med . relkitSubst

-- | Create @alt@ relkit.
relkitSubst :: (D.CContent c) => (D.CopSet c, [D.NamedCox c]) -> C.RelkitFlow c
relkitSubst _ Nothing = Right C.relkitNothing
relkitSubst (cops, cox) (Just he1)
    | B.sameLength ns ind = Right kit2
    | otherwise           = Msg.unexpTermName
    where
      (ns, xs)  =  unzip cox               -- names and expressions
      ns1       =  D.getTermNames he1      -- term names of input relation
      ind       =  ns `B.snipIndex` ns1    -- indicies for ns on input relation
      cut       =  B.snipOff  ind          -- cutting-ns function
      fore      =  B.snipForward ind       -- cutting-ns function
      he2       =  D.headMap fore he1      -- heading of output relation
      kit2      =  C.relkitJust he2 $ C.RelkitOneToAbOne True f2 []
      f2 _ cs1  =  do cs2 <- D.coxRunCox cops he1 cs1 `mapM` xs
                      Right $ cs2 ++ cut cs1


-- ----------------------  fill

-- | __fill \/P -with E__
consFill :: (D.CContent c) => C.RopCons c
consFill med =
  do ns    <- Op.getTerms med "-term"
     coxTo <- Op.getCox med "-with"
     let cops = C.globalCopset $ C.ropGlobal med
     Right $ relmapFill med (ns, cops, coxTo)

-- | Create @fill@ relmap.
relmapFill :: (D.CContent c) => C.Intmed c -> ([S.TermName], D.CopSet c, D.Cox c) -> C.Relmap c
relmapFill med = C.relmapFlow med . relkitFill

-- | Create @fill@ relkit.
relkitFill :: (D.CContent c) => ([S.TermName], D.CopSet c, D.Cox c) -> C.RelkitFlow c
relkitFill _ Nothing = Right C.relkitNothing
relkitFill (ns, cops, coxTo) (Just he1) = Right kit2 where
    ns1       =  D.getTermNames he1
    ind       =  ns `B.snipIndex` ns1
    pick      =  B.snipFrom ind
    cut       =  B.snipOff  ind
    fore      =  B.snipForward ind
    he2       =  D.headMap fore he1
    kit2      =  C.relkitJust he2 $ C.RelkitOneToAbOne True f2 []
    f2 _ cs1  =  do cTo  <- D.coxRunCox cops he1 cs1 coxTo
                    let fill c | D.isEmpty c = cTo
                               | otherwise   = c
                    Right $ map fill (pick cs1) ++ (cut cs1)


-- ----------------------  replace

-- | __map \/P ... -by F__
consReplace :: (D.CContent c) => C.RopCons c
consReplace med =
    do ns     <- Op.getTerms med "-term"
       coxBy  <- Op.getCox med "-by"
       B.unless (D.coxSyntacticArity coxBy == 1) $ do
         B.abortable "relmap-replace" [coxBy] Msg.reqUnaryFn
       let expr n = (n, D.CoxFill [] coxBy [D.CoxTerm [] [n] []])
           cops   = C.globalCopset $ C.ropGlobal med
       Right $ relmapSubst med (cops, map expr ns)


-- ----------------------  replace-all

-- | __replace-all -from E -to E__
consReplaceAll :: (D.CContent c) => C.RopCons c
consReplaceAll med =
  do coxFrom <- Op.getCox med "-from"
     coxTo   <- Op.getCox med "-to"
     let cops = C.globalCopset $ C.ropGlobal med
     Right $ relmapReplaceAll med (cops, coxFrom, coxTo)

-- | Create @replace-all@ relmap.
relmapReplaceAll :: (D.CContent c) => C.Intmed c -> (D.CopSet c, D.Cox c, D.Cox c) -> C.Relmap c
relmapReplaceAll med = C.relmapFlow med . relkitReplaceAll

-- | Create @replace-all@ relkit.
relkitReplaceAll :: (D.CContent c) => (D.CopSet c, D.Cox c, D.Cox c) -> C.RelkitFlow c
relkitReplaceAll _ Nothing = Right C.relkitNothing
relkitReplaceAll (cops, coxFrom, coxTo) (Just he1) = Right kit2 where
    kit2     = C.relkitJust he1 $ C.RelkitOneToAbOne False f2 []
    f2 _ cs  = do cFrom    <-  D.coxRunCox cops he1 cs coxFrom
                  cTo      <-  D.coxRunCox cops he1 cs coxTo
                  let replace c | c == cFrom = cTo
                                | otherwise  = c
                  Right $ map replace cs


-- ----------------------  split

-- | __split \/N E ...__
consSplit :: (D.CContent c) => C.RopCons c
consSplit med =
    do cops <- Op.getWhere med "-where"
       cox <- Op.getTermCoxes med "-cox"
       Right $ relmapSplit med (cops, cox)

-- | Create @split@ relmap.
relmapSplit :: (D.CContent c) => C.Intmed c -> (D.CopSet c, [D.NamedCox c]) -> C.Relmap c
relmapSplit med = C.relmapFlow med . relkitSplit

-- | Create @split@ relkit.
relkitSplit :: forall c. (D.CContent c) => (D.CopSet c, [D.NamedCox c]) -> C.RelkitFlow c
relkitSplit _ Nothing = Right C.relkitNothing
relkitSplit (cops, cox) (Just he1)
    | null ind  = Right kit2
    | otherwise = Msg.unexpTermName
    where
      (ns, xs)    =  unzip cox               -- names and expressions
      ns1         =  D.getTermNames he1      -- term names
      ind         =  ns `B.snipIndex` ns1    -- shared indicies

      he2         =  D.headNests ns he1
      kit2        =  C.relkitJust he2 $ C.RelkitAbFull False kitf2 []
      kitf2 _ bo1 =  do let fs2 = D.coxRunList cops he1 `map` xs
                        cs2 <- split fs2 bo1
                        Right [cs2]

      split :: [D.RunList c] -> [[c]] -> B.Ab [c]
      split [] _ = Right []
      split (f : fs2) bo1 =
          do bo2 <- run f `mapM` bo1
             let first = filter fst bo2
                 rest  = map snd $ B.omit fst bo2
             rest' <- split fs2 rest
             let rel = D.Rel he1 $ map snd first
             Right $ D.pRel rel : rest'

      run :: D.RunList c -> [c] -> B.Ab (Bool, [c])
      run f cs = do c <- f cs
                    case D.isBool c of
                      True  -> Right (D.gBool c, cs)
                      False -> Msg.reqBool


-- ----------------------  unary

-- | __unary \/N E : ...__
consUnary :: (D.CContent c) => C.RopCons c
consUnary med =
    do n  <- Op.getTerm  med "-term"
       cs <- Op.getContents med "-expr"
       Right $ relmapUnary med (n, cs)

-- | Create @unary@ relmap.
relmapUnary :: (D.CContent c) => C.Intmed c -> (S.TermName, [c]) -> C.Relmap c
relmapUnary med = C.relmapFlow med . relkitUnary

-- | Create @unary@ relkit.
relkitUnary :: (D.CContent c) => (S.TermName, [c]) -> C.RelkitFlow c
relkitUnary (n, cs) _ = Right kit2 where
    he2    = D.headFrom [n]
    kit2   = C.relkitJust he2 $ C.RelkitAbFull True f2 []
    f2 _ _ = Right $ map B.li1 cs



-- ----------------------  dump-cox

-- | __dump-cox E__
consDumpCox :: (D.CContent c) => C.RopCons c
consDumpCox med =
    do cox <- Op.getCox med "-cox"
       Msg.dumpCox cox


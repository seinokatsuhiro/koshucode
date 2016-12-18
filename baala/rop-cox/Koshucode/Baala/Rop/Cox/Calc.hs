{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Relmap operator with content calculation.

module Koshucode.Baala.Rop.Cox.Calc
  ( ropsCoxCalc,
    -- * add
    consAdd, relmapAdd,
    -- * alt
    consAlt, relmapAlt,
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
import Koshucode.Baala.Overture ((&))
import qualified Koshucode.Baala.Overture        as O
import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Syntax          as S
import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Rop.Base        as Rop
import qualified Koshucode.Baala.Rop.Cox.Get     as Rop
import qualified Koshucode.Baala.Rop.Cox.Message as Msg


-- | Implementation of relational operators.
ropsCoxCalc :: (D.CContent c) => [C.Rop c]
ropsCoxCalc = Rop.ropAlias
    [ "subst" & "alt"
    ] $ Rop.ropList "cox-calc"
    --        CONSTRUCTOR       USAGE                            ATTRIBUTE
    [ Rop.rop consAdd         [ "add /N E ..."               O.& "-cox* . -where?" ]
    , Rop.rop consAlt         [ "alt /N E ..."               O.& "-cox* . -where?" ]
    , Rop.rop consFill        [ "fill /P ... -with E"        O.& "-term* . -with" ]
    , Rop.rop consReplace     [ "replace /P ... -by F"       O.& "-term* . -by" ]
    , Rop.rop consReplaceAll  [ "replace-all -from E -to E"  O.& ". -from -to" ]
    , Rop.rop consSplit       [ "split /N E ..."             O.& "-cox* . -where?" ]
    , Rop.rop consUnary       [ "unary /N E ..."             O.& "-term -expr*" ]
    , Rop.rop consDumpCox     [ "dump-cox E"                 O.& "-cox*" ]
    ]


-- ----------------------  add

-- | __add \/N E ...__
--
--   Add new terms \/N which has the result of E.
--
consAdd :: (D.CContent c) => C.RopCons c
consAdd med =
    do cops <- Rop.getWhere med "-where"
       cox  <- Rop.getTermCoxes med "-cox"
       Right $ relmapAdd med (cops, cox)

-- | Create @add@ relmap.
relmapAdd :: (D.CContent c) => C.Intmed c -> (D.CopSet c, [S.Term (D.Cox c)]) -> C.Relmap c
relmapAdd med = C.relmapFlow med . relkitAdd

-- | Create @add@ relkit.
relkitAdd :: (D.CContent c) => (D.CopSet c, [S.Term (D.Cox c)]) -> C.RelkitFlow c
relkitAdd _ Nothing = Right C.relkitNothing
relkitAdd (cops, cox) (Just he1)
    | B.duplicated ns     = Msg.dupTerm ns
    | D.preTermsExist pk  = Msg.reqNewTerm (D.ssLShareNames pk) he1
    | otherwise           = Right kit2
    where
      (ns, xs)   = unzip cox       -- names and expressions
      pk         = D.termPicker ns he1
      he2        = ns `D.headAppend` he1
      kit2       = C.relkitJust he2 $ C.RelkitAbLinear False f2 []
      f2 _ cs1   = do cs2 <- D.coxRunCox cops he1 cs1 `mapM` xs
                      Right $ cs2 ++ cs1


-- ----------------------  alt

-- | __alt \/P E ...__
--
--   Change present terms \/P whose content is altered
--   to the result of E.
--
consAlt :: (D.CContent c) => C.RopCons c
consAlt med =
    do cops <- Rop.getWhere med "-where"
       cox  <- Rop.getTermCoxes med "-cox"
       Right $ relmapAlt med (cops, cox)

-- | Create @alt@ relmap.
relmapAlt :: (D.CContent c) => C.Intmed c -> (D.CopSet c, [S.Term (D.Cox c)]) -> C.Relmap c
relmapAlt med = C.relmapFlow med . relkitAlt

-- | Create @alt@ relkit.
relkitAlt :: (D.CContent c) => (D.CopSet c, [S.Term (D.Cox c)]) -> C.RelkitFlow c
relkitAlt _ Nothing = Right C.relkitNothing
relkitAlt (cops, cox) (Just he1)
    | B.duplicated ns     = Msg.dupTerm ns
    | D.newTermsExist pk  = Msg.unkTerm (D.newTerms pk) he1
    | otherwise           = Right kit2
    where
      (ns, xs)  = unzip cox               -- names and expressions
      pk        = D.termPicker ns he1
      he2       = D.forwardTerms pk `D.headMap` he1  -- heading of output relation
      kit2      = C.relkitJust he2 $ C.RelkitAbLinear True f2 []
      f2 _ cs1  = do cs2 <- D.coxRunCox cops he1 cs1 `mapM` xs
                     Right $ cs2 ++ D.cutTerms pk cs1


-- ----------------------  fill

-- | __fill \/P ... -with E__
--
--   Fill terms \/P ... with the result of E.
--
consFill :: (D.CContent c) => C.RopCons c
consFill med =
  do ns    <- Rop.getTerms med "-term"
     coxTo <- Rop.getCox med "-with"
     let cops = C.globalCopset $ C.ropGlobal med
     Right $ relmapFill med (ns, cops, coxTo)

-- | Create @fill@ relmap.
relmapFill :: (D.CContent c) => C.Intmed c -> ([S.TermName], D.CopSet c, D.Cox c) -> C.Relmap c
relmapFill med = C.relmapFlow med . relkitFill

-- | Create @fill@ relkit.
relkitFill :: (D.CContent c) => ([S.TermName], D.CopSet c, D.Cox c) -> C.RelkitFlow c
relkitFill _ Nothing = Right C.relkitNothing
relkitFill (ns, cops, coxTo) (Just he1)
    | B.duplicated ns     = Msg.dupTerm ns
    | D.newTermsExist pk  = Msg.unkTerm (D.newTerms pk) he1
    | otherwise           = Right kit2
    where
      pk        = D.termPicker ns he1
      he2       = D.forwardTerms pk `D.headMap` he1
      kit2      = C.relkitJust he2 $ C.RelkitAbLinear True f2 []
      f2 _ cs1  = do cTo  <- D.coxRunCox cops he1 cs1 coxTo
                     let fill c | D.isEmpty c = cTo
                                | otherwise   = c
                     Right $ map fill (D.pickTerms pk cs1) ++ (D.cutTerms pk cs1)


-- ----------------------  replace

-- | __map \/P ... -by F__
consReplace :: (D.CContent c) => C.RopCons c
consReplace med =
    do ns     <- Rop.getTerms med "-term"
       coxBy  <- Rop.getCox med "-by"
       B.unless (D.coxSyntacticArity coxBy == 1) $ do
         B.abortable "relmap-replace" [coxBy] Msg.reqUnaryFn
       let expr n = (n, D.CoxFill [] coxBy [D.CoxTerm [] [n] []])
           cops   = C.globalCopset $ C.ropGlobal med
       Right $ relmapAlt med (cops, map expr ns)


-- ----------------------  replace-all

-- | __replace-all -from E -to E__
consReplaceAll :: (D.CContent c) => C.RopCons c
consReplaceAll med =
  do coxFrom <- Rop.getCox med "-from"
     coxTo   <- Rop.getCox med "-to"
     let cops = C.globalCopset $ C.ropGlobal med
     Right $ relmapReplaceAll med (cops, coxFrom, coxTo)

-- | Create @replace-all@ relmap.
relmapReplaceAll :: (D.CContent c) => C.Intmed c -> (D.CopSet c, D.Cox c, D.Cox c) -> C.Relmap c
relmapReplaceAll med = C.relmapFlow med . relkitReplaceAll

-- | Create @replace-all@ relkit.
relkitReplaceAll :: (D.CContent c) => (D.CopSet c, D.Cox c, D.Cox c) -> C.RelkitFlow c
relkitReplaceAll _ Nothing = Right C.relkitNothing
relkitReplaceAll (cops, coxFrom, coxTo) (Just he1) = Right kit2 where
    kit2     = C.relkitJust he1 $ C.RelkitAbLinear False f2 []
    f2 _ cs  = do cFrom    <-  D.coxRunCox cops he1 cs coxFrom
                  cTo      <-  D.coxRunCox cops he1 cs coxTo
                  let replace c | c == cFrom = cTo
                                | otherwise  = c
                  Right $ map replace cs


-- ----------------------  split

-- | __split \/N E ...__
--
--   Split input relation by the boolean functions E ...,
--   and add terms \/N ... which has the splitted relations.
--
consSplit :: (D.CContent c) => C.RopCons c
consSplit med =
    do cops <- Rop.getWhere med "-where"
       cox <- Rop.getTermCoxes med "-cox"
       Right $ relmapSplit med (cops, cox)

-- | Create @split@ relmap.
relmapSplit :: (D.CContent c) => C.Intmed c -> (D.CopSet c, [S.Term (D.Cox c)]) -> C.Relmap c
relmapSplit med = C.relmapFlow med . relkitSplit

-- | Create @split@ relkit.
relkitSplit :: forall c. (D.CContent c) => (D.CopSet c, [S.Term (D.Cox c)]) -> C.RelkitFlow c
relkitSplit _ Nothing = Right C.relkitNothing
relkitSplit (cops, cox) (Just he1)
    | B.duplicated ns     = Msg.dupTerm ns
    | D.preTermsExist pk  = Msg.reqNewTerm (D.ssLShareNames pk) he1
    | otherwise           = Right kit2
    where
      (ns, xs)   = unzip cox               -- names and expressions
      pk         = D.termPicker ns he1
      he2        = D.headNests ns he1
      kit2       = C.relkitJust he2 $ C.RelkitAbFull False f2 []
      f2 _ bo1   = do let fs2 = D.coxRunList cops he1 `map` xs
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
    do n  <- Rop.getTerm  med "-term"
       cs <- Rop.getContents med "-expr"
       Right $ relmapUnary med (n, cs)

-- | Create @unary@ relmap.
relmapUnary :: (D.CContent c) => C.Intmed c -> (S.TermName, [c]) -> C.Relmap c
relmapUnary med = C.relmapFlow med . relkitUnary

-- | Create @unary@ relkit.
relkitUnary :: (D.CContent c) => (S.TermName, [c]) -> C.RelkitFlow c
relkitUnary (n, cs) _ = Right kit2 where
    he2    = D.headFrom [n]
    kit2   = C.relkitJust he2 $ C.RelkitAbFull True f2 []
    f2 _ _ = Right $ map B.list1 cs



-- ----------------------  dump-cox

-- | __dump-cox E__
consDumpCox :: (D.CContent c) => C.RopCons c
consDumpCox med =
    do cox <- Rop.getCox med "-cox"
       Msg.dumpCox cox


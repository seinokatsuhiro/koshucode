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

import qualified Koshucode.Baala.DataPlus        as K
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Rop.Base        as Rop
import qualified Koshucode.Baala.Rop.Cox.Message as Msg


-- | Implementation of relational operators.
ropsCoxCalc :: (K.CContent c) => [C.Rop c]
ropsCoxCalc = Rop.ropAlias
    [ "subst" K.& "alt"
    ] $ Rop.rops "cox-calc"
    [ consAdd         K.& [ "add /N E ..."               K.& "-cox* . -where?" ]
    , consAlt         K.& [ "alt /N E ..."               K.& "-cox* . -where?" ]
    , consFill        K.& [ "fill /P ... -with E"        K.& "-term* . -with" ]
    , consReplace     K.& [ "replace /P ... -by F"       K.& "-term* . -by" ]
    , consReplaceAll  K.& [ "replace-all -from E -to E"  K.& ". -from -to" ]
    , consSplit       K.& [ "split /N E ..."             K.& "-cox* . -where?" ]
    , consUnary       K.& [ "unary /N E ..."             K.& "-term -expr*" ]
    , consDumpCox     K.& [ "dump-cox E"                 K.& "-cox*" ]
    ]


-- ----------------------  add

-- | __add \/N E ...__
--
--   Add new terms \/N which has the result of E.
--
consAdd :: (K.CContent c) => C.RopCons c
consAdd med =
    do cops <- Rop.getWhere med "-where"
       cox  <- Rop.getTermCoxes med "-cox"
       Right $ relmapAdd med (cops, cox)

-- | Create @add@ relmap.
relmapAdd :: (K.CContent c) => C.Intmed c -> (K.CopSet c, [K.Term (K.Cox c)]) -> C.Relmap c
relmapAdd med = C.relmapFlow med . relkitAdd

-- | Create @add@ relkit.
relkitAdd :: (K.CContent c) => (K.CopSet c, [K.Term (K.Cox c)]) -> C.RelkitFlow c
relkitAdd _ Nothing = Right C.relkitNothing
relkitAdd (cops, cox) (Just he1)
    | K.duplicated ns     = Msg.dupTerm ns
    | K.preTermsExist pk  = Msg.reqNewTerm (K.pkLShareNames pk) he1
    | otherwise           = Right kit2
    where
      (ns, xs)   = unzip cox       -- names and expressions
      pk         = K.termPicker ns he1
      he2        = ns `K.headAppend` he1
      kit2       = C.relkitJust he2 $ C.RelkitAbLinear False f2 []
      f2 _ cs1   = do cs2 <- K.coxRunCox cops he1 cs1 `mapM` xs
                      Right $ cs2 ++ cs1


-- ----------------------  alt

-- | __alt \/P E ...__
--
--   Change present terms \/P whose content is altered
--   to the result of E.
--
consAlt :: (K.CContent c) => C.RopCons c
consAlt med =
    do cops <- Rop.getWhere med "-where"
       cox  <- Rop.getTermCoxes med "-cox"
       Right $ relmapAlt med (cops, cox)

-- | Create @alt@ relmap.
relmapAlt :: (K.CContent c) => C.Intmed c -> (K.CopSet c, [K.Term (K.Cox c)]) -> C.Relmap c
relmapAlt med = C.relmapFlow med . relkitAlt

-- | Create @alt@ relkit.
relkitAlt :: (K.CContent c) => (K.CopSet c, [K.Term (K.Cox c)]) -> C.RelkitFlow c
relkitAlt _ Nothing = Right C.relkitNothing
relkitAlt (cops, cox) (Just he1)
    | K.duplicated ns     = Msg.dupTerm ns
    | K.newTermsExist pk  = Msg.unkTerm (K.newTerms pk) he1
    | otherwise           = Right kit2
    where
      (ns, xs)  = unzip cox               -- names and expressions
      pk        = K.termPicker ns he1
      he2       = K.forwardTerms pk `K.headMap` he1  -- heading of output relation
      kit2      = C.relkitJust he2 $ C.RelkitAbLinear True f2 []
      f2 _ cs1  = do cs2 <- K.coxRunCox cops he1 cs1 `mapM` xs
                     Right $ cs2 ++ K.cutTerms pk cs1


-- ----------------------  fill

-- | __fill \/P ... -with E__
--
--   Fill terms \/P ... with the result of E.
--
consFill :: (K.CContent c) => C.RopCons c
consFill med =
  do ns    <- Rop.getTerms med "-term"
     coxTo <- Rop.getCox med "-with"
     let cops = C.globalCopset $ C.ropGlobal med
     Right $ relmapFill med (ns, cops, coxTo)

-- | Create @fill@ relmap.
relmapFill :: (K.CContent c) => C.Intmed c -> ([K.TermName], K.CopSet c, K.Cox c) -> C.Relmap c
relmapFill med = C.relmapFlow med . relkitFill

-- | Create @fill@ relkit.
relkitFill :: (K.CContent c) => ([K.TermName], K.CopSet c, K.Cox c) -> C.RelkitFlow c
relkitFill _ Nothing = Right C.relkitNothing
relkitFill (ns, cops, coxTo) (Just he1)
    | K.duplicated ns     = Msg.dupTerm ns
    | K.newTermsExist pk  = Msg.unkTerm (K.newTerms pk) he1
    | otherwise           = Right kit2
    where
      pk        = K.termPicker ns he1
      he2       = K.forwardTerms pk `K.headMap` he1
      kit2      = C.relkitJust he2 $ C.RelkitAbLinear True f2 []
      f2 _ cs1  = do cTo  <- K.coxRunCox cops he1 cs1 coxTo
                     let fill c | K.isEmpty c = cTo
                                | otherwise   = c
                     Right $ map fill (K.pickTerms pk cs1) ++ (K.cutTerms pk cs1)


-- ----------------------  replace

-- | __map \/P ... -by F__
consReplace :: (K.CContent c) => C.RopCons c
consReplace med =
    do ns     <- Rop.getTerms med "-term"
       coxBy  <- Rop.getCox med "-by"
       K.unless (K.coxSyntacticArity coxBy == 1) $ do
         K.abortable "relmap-replace" [coxBy] Msg.reqUnaryFn
       let expr n = (n, K.CoxFill [] coxBy [K.CoxTerm [] [n] []])
           cops   = C.globalCopset $ C.ropGlobal med
       Right $ relmapAlt med (cops, map expr ns)


-- ----------------------  replace-all

-- | __replace-all -from E -to E__
consReplaceAll :: (K.CContent c) => C.RopCons c
consReplaceAll med =
  do coxFrom <- Rop.getCox med "-from"
     coxTo   <- Rop.getCox med "-to"
     let cops = C.globalCopset $ C.ropGlobal med
     Right $ relmapReplaceAll med (cops, coxFrom, coxTo)

-- | Create @replace-all@ relmap.
relmapReplaceAll :: (K.CContent c) => C.Intmed c -> (K.CopSet c, K.Cox c, K.Cox c) -> C.Relmap c
relmapReplaceAll med = C.relmapFlow med . relkitReplaceAll

-- | Create @replace-all@ relkit.
relkitReplaceAll :: (K.CContent c) => (K.CopSet c, K.Cox c, K.Cox c) -> C.RelkitFlow c
relkitReplaceAll _ Nothing = Right C.relkitNothing
relkitReplaceAll (cops, coxFrom, coxTo) (Just he1) = Right kit2 where
    kit2     = C.relkitJust he1 $ C.RelkitAbLinear False f2 []
    f2 _ cs  = do cFrom    <-  K.coxRunCox cops he1 cs coxFrom
                  cTo      <-  K.coxRunCox cops he1 cs coxTo
                  let replace c | c == cFrom = cTo
                                | otherwise  = c
                  Right $ map replace cs


-- ----------------------  split

-- | __split \/N E ...__
--
--   Split input relation by the boolean functions E ...,
--   and add terms \/N ... which has the splitted relations.
--
consSplit :: (K.CContent c) => C.RopCons c
consSplit med =
    do cops <- Rop.getWhere med "-where"
       cox <- Rop.getTermCoxes med "-cox"
       Right $ relmapSplit med (cops, cox)

-- | Create @split@ relmap.
relmapSplit :: (K.CContent c) => C.Intmed c -> (K.CopSet c, [K.Term (K.Cox c)]) -> C.Relmap c
relmapSplit med = C.relmapFlow med . relkitSplit

-- | Create @split@ relkit.
relkitSplit :: forall c. (K.CContent c) => (K.CopSet c, [K.Term (K.Cox c)]) -> C.RelkitFlow c
relkitSplit _ Nothing = Right C.relkitNothing
relkitSplit (cops, cox) (Just he1)
    | K.duplicated ns     = Msg.dupTerm ns
    | K.preTermsExist pk  = Msg.reqNewTerm (K.pkLShareNames pk) he1
    | otherwise           = Right kit2
    where
      (ns, xs)   = unzip cox               -- names and expressions
      pk         = K.termPicker ns he1
      he2        = K.headNests ns he1
      kit2       = C.relkitJust he2 $ C.RelkitAbFull False f2 []
      f2 _ bo1   = do let fs2 = K.coxRunList cops he1 `map` xs
                      cs2 <- split fs2 bo1
                      Right [cs2]

      split :: [K.RunList c] -> [[c]] -> K.Ab [c]
      split [] _ = Right []
      split (f : fs2) bo1 =
          do bo2 <- run f `mapM` bo1
             let first = filter fst bo2
                 rest  = map snd $ K.omit fst bo2
             rest' <- split fs2 rest
             let rel = K.Rel he1 $ map snd first
             Right $ K.pRel rel : rest'

      run :: K.RunList c -> [c] -> K.Ab (Bool, [c])
      run f cs = do c <- f cs
                    case K.isBool c of
                      True  -> Right (K.gBool c, cs)
                      False -> Msg.reqBool


-- ----------------------  unary

-- | __unary \/N E : ...__
consUnary :: (K.CContent c) => C.RopCons c
consUnary med =
    do n  <- Rop.getTerm  med "-term"
       cs <- Rop.getContents med "-expr"
       Right $ relmapUnary med (n, cs)

-- | Create @unary@ relmap.
relmapUnary :: (K.CContent c) => C.Intmed c -> (K.TermName, [c]) -> C.Relmap c
relmapUnary med = C.relmapFlow med . relkitUnary

-- | Create @unary@ relkit.
relkitUnary :: (K.CContent c) => (K.TermName, [c]) -> C.RelkitFlow c
relkitUnary (n, cs) _ = Right kit2 where
    he2    = K.headFrom [n]
    kit2   = C.relkitJust he2 $ C.RelkitAbFull True f2 []
    f2 _ _ = Right $ map K.list1 cs



-- ----------------------  dump-cox

-- | __dump-cox E__
consDumpCox :: (K.CContent c) => C.RopCons c
consDumpCox med =
    do cox <- Rop.getCox med "-cox"
       Msg.dumpCox cox


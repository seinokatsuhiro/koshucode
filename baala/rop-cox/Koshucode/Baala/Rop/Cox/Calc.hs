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
    -- * dump-cox
    consDumpCox,
  ) where

import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Base.Message  as Msg


-- | Implementation of relational operators.
ropsCoxCalc :: (K.CContent c) => [C.Rop c]
ropsCoxCalc = Rop.ropAlias
    [ "subst" K.& "alt"
    ] $ Rop.rops "cox-calc"
    [ consAdd         K.& [ "add /N E ..."               K.& "-cox* . -let?" ]
    , consAlt         K.& [ "alt /N E ..."               K.& "-cox* . -let?" ]
    , consFill        K.& [ "fill /P ... -with E"        K.& "-term* . -with" ]
    , consReplace     K.& [ "replace /P ... -by F"       K.& "-term* . -by" ]
    , consReplaceAll  K.& [ "replace-all -from E -to E"  K.& ". -from -to" ]
    , consSplit       K.& [ "split /N E ..."             K.& "-cox* . -let?" ]
    , consUnary       K.& [ "unary /N E ..."             K.& "-term -expr*" ]
    , consDumpCox     K.& [ "dump-cox E"                 K.& "-cox*" ]
    ]


-- ----------------------  add

-- | [add /\/N/ /E/ ... -let { /D/ | ... } ]
--     Calculate expression /E/ of term /\/N/,
--     and add terms /\/N/ ... to input relation.
--
consAdd :: (K.CContent c) => C.RopCons c
consAdd med =
    do cops <- Rop.getLet med "-let"
       cox  <- Rop.getCoxTerms med "-cox"
       Right $ relmapAdd med (cops, cox)

-- | Create @add@ relmap.
relmapAdd :: (K.CContent c) => C.Intmed c -> (K.CopSet c, [K.TermCox c]) -> C.Relmap c
relmapAdd med = C.relmapFlow med . relkitAdd

-- | Create @add@ relkit.
relkitAdd :: (K.CContent c) => (K.CopSet c, [K.TermCox c]) -> C.RelkitFlow c
relkitAdd _ Nothing = C.relkitUnfixed
relkitAdd (cops, cox) (Just he1) = Rop.newCheck pk kit where
    (ns, xs)  = unzip cox    -- terms and expressions
    pk        = K.termPicker ns he1
    he2       = ns `K.headAppend` he1
    kit       = Right $ C.relkitLineAb False he2 flow
    flow cs1  = do cs2 <- K.coxRunCox cops he1 cs1 K.<#> xs
                   Right $ cs2 ++ cs1


-- ----------------------  alt

-- | [alt /\/P/ /E/ ... -let { /D/ | ... }]
--     Calculate expression /E/ of term /\/P/,
--     and alter the contents of present terms /\/P/ ....
--
consAlt :: (K.CContent c) => C.RopCons c
consAlt med =
    do cops <- Rop.getLet med "-let"
       cox  <- Rop.getCoxTerms med "-cox"
       Right $ relmapAlt med (cops, cox)

-- | Create @alt@ relmap.
relmapAlt :: (K.CContent c) => C.Intmed c -> (K.CopSet c, [K.TermCox c]) -> C.Relmap c
relmapAlt med = C.relmapFlow med . relkitAlt

-- | Create @alt@ relkit.
relkitAlt :: (K.CContent c) => (K.CopSet c, [K.TermCox c]) -> C.RelkitFlow c
relkitAlt _ Nothing = C.relkitUnfixed
relkitAlt (cops, cox) (Just he1) = Rop.preCheck pk kit where
    (ns, xs)  = unzip cox
    pk        = K.termPicker ns he1
    he2       = K.forwardTerms pk `K.headMap` he1
    kit       = Right $ C.relkitLineAb True he2 flow
    flow cs1  = do cs2 <- K.coxRunCox cops he1 cs1 K.<#> xs
                   Right $ cs2 ++ K.cutTerms pk cs1


-- ----------------------  fill

-- | [fill /\/P/ ... -with /E/]
--     Fill contents of present terms /\/P/ ...
--     with the result of /E/.
--
consFill :: (K.CContent c) => C.RopCons c
consFill med =
  do ns    <- Rop.getTerms med "-term"
     coxTo <- Rop.getCox med "-with"
     let cops = C.globalCopset $ C.getGlobal med
     Right $ relmapFill med (ns, cops, coxTo)

-- | Create @fill@ relmap.
relmapFill :: (K.CContent c) => C.Intmed c -> ([K.TermName], K.CopSet c, K.Cox c) -> C.Relmap c
relmapFill med = C.relmapFlow med . relkitFill

-- | Create @fill@ relkit.
relkitFill :: (K.CContent c) => ([K.TermName], K.CopSet c, K.Cox c) -> C.RelkitFlow c
relkitFill _ Nothing = C.relkitUnfixed
relkitFill (ns, cops, coxTo) (Just he1) = Rop.preCheck pk kit where
    pk        = K.termPicker ns he1
    he2       = K.forwardTerms pk `K.headMap` he1
    kit       = Right $ C.relkitLineAb True he2 flow
    flow cs1  = do cTo <- K.coxRunCox cops he1 cs1 coxTo
                   let fill c | K.isEmpty c = cTo
                              | otherwise   = c
                   Right $ (fill <$> K.pickTerms pk cs1) ++ (K.cutTerms pk cs1)


-- ----------------------  replace

-- | __map \/P ... -by F__
consReplace :: (K.CContent c) => C.RopCons c
consReplace med =
    do ns     <- Rop.getTerms med "-term"
       coxBy  <- Rop.getCox med "-by"
       K.unless (K.coxSyntacticArity coxBy == 1) $ do
         K.abortable "relmap-replace" [coxBy] Msg.reqUnaryFn
       let expr n = (n, K.CoxFill [] coxBy [K.CoxTerm [] [n] []])
           cops   = C.globalCopset $ C.getGlobal med
       Right $ relmapAlt med (cops, map expr ns)


-- ----------------------  replace-all

-- | __replace-all -from E -to E__
consReplaceAll :: (K.CContent c) => C.RopCons c
consReplaceAll med =
  do coxFrom <- Rop.getCox med "-from"
     coxTo   <- Rop.getCox med "-to"
     let cops = C.globalCopset $ C.getGlobal med
     Right $ relmapReplaceAll med (cops, coxFrom, coxTo)

-- | Create @replace-all@ relmap.
relmapReplaceAll :: (K.CContent c) => C.Intmed c -> (K.CopSet c, K.Cox c, K.Cox c) -> C.Relmap c
relmapReplaceAll med = C.relmapFlow med . relkitReplaceAll

-- | Create @replace-all@ relkit.
relkitReplaceAll :: (K.CContent c) => (K.CopSet c, K.Cox c, K.Cox c) -> C.RelkitFlow c
relkitReplaceAll _ Nothing = C.relkitUnfixed
relkitReplaceAll (cops, coxFrom, coxTo) (Just he1) = Right kit2 where
    kit2     = C.relkitLineAb False he1 flow
    flow cs  = do cFrom  <- K.coxRunCox cops he1 cs coxFrom
                  cTo    <- K.coxRunCox cops he1 cs coxTo
                  let replace c | c == cFrom = cTo
                                | otherwise  = c
                  Right $ map replace cs


-- ----------------------  split

-- | [split /\/N E/ ... -let { /D/ | ... }]
--     Split input relation by the boolean functions /E/ ...,
--     and add terms /\/N/ ... which has the splitted relations.
--
--   > RELMAP
--   >   split /a ( /x = 1 )
--   >         /b ( /y = 10 )
--   > 
--   > INPUT              OUTPUT
--   >    /x    /y          /a          /b
--   >    ----- -----       ----------- -----------
--   >    1     10          /x    /y    /x    /y
--   >    2     20          ----- ----- ----- -----
--   >    3     10          1     10    3     10
--   >    1     20          1     20
--   >    2     30
--
consSplit :: (K.CContent c) => C.RopCons c
consSplit med =
    do cops <- Rop.getLet med "-let"
       cox  <- Rop.getCoxTerms med "-cox"
       Right $ relmapSplit med (cops, cox)

-- | Create @split@ relmap.
relmapSplit :: (K.CContent c) => C.Intmed c -> (K.CopSet c, [K.TermCox c]) -> C.Relmap c
relmapSplit med = C.relmapFlow med . relkitSplit

-- | Create @split@ relkit.
relkitSplit :: forall c. (K.CContent c) => (K.CopSet c, [K.TermCox c]) -> C.RelkitFlow c
relkitSplit _ Nothing = C.relkitUnfixed
relkitSplit (cops, cox) (Just he1) = Rop.newCheck pk kit where
    (ns, xs)   = unzip cox               -- names and expressions
    pk         = K.termPicker ns he1
    he2        = K.headNests ns he1
    kit        = Right $ C.relkitWholeAb False he2 flow
    flow bo    = do let fs = K.calcTuple cops he1 <$> xs
                    cs2 <- split fs bo
                    Right [cs2]

    split :: [K.CalcTuple c] -> [[c]] -> K.Ab [c]
    split [] _ = Right []
    split (f : fs) bo1 =
        do bo2 <- run f `mapM` bo1
           let first = filter fst bo2
               rest  = map snd $ K.omit fst bo2
           rest' <- split fs rest
           let rel = K.Rel he1 $ map snd first
           Right $ K.pRel rel : rest'

    run :: K.CalcTuple c -> [c] -> K.Ab (Bool, [c])
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
    kit2   = C.relkitWholeAb True he2 flow
    flow _ = Right $ map K.list1 cs



-- ----------------------  dump-cox

-- | [dump-cox /E/]
consDumpCox :: (K.CContent c) => C.RopCons c
consDumpCox med =
    do cox <- Rop.getCox med "-cox"
       Msg.dumpCox cox


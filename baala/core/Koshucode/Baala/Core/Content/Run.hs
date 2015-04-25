{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content calcutation.

module Koshucode.Baala.Core.Content.Run
  ( RunCox, RunList,
    coxRunCox, coxRunList,
    calcContent,
  
    -- * Getting arguments
    getArg1, getArg2, getArg3,
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Content.Build     as C
import qualified Koshucode.Baala.Core.Content.Class     as C
import qualified Koshucode.Baala.Core.Content.Cop       as C
import qualified Koshucode.Baala.Core.Content.Cox       as C
import qualified Koshucode.Baala.Core.Content.Literal   as C
import qualified Koshucode.Baala.Core.Message           as Msg


-- --------------------------------------------  Beta reduction

data Beta c
    = BetaLit  [B.CodePt] c                     -- ^ Literal content
    | BetaTerm [B.CodePt] [B.TermName] [Int]    -- ^ Term reference, its name and position
    | BetaCall [B.CodePt] B.BlankName (C.CopCalc c) [B.Ab (Beta c)]  -- ^ Function application

instance B.CodePtr (Beta c) where
    codePtList (BetaLit  cp _)      = cp
    codePtList (BetaTerm cp _ _)    = cp
    codePtList (BetaCall cp _ _ _)  = cp

-- | Reduce content expression.
beta :: (B.Write c) => C.CopSet c -> B.Head -> C.Cox c -> B.Ab (Beta c)
beta copset he cox =
    do let deriv = C.copsetDerived copset
       deriv2  <- B.sequenceSnd $ B.mapSndTo pos deriv
       cox2    <- pos cox                   -- put term index
       let cox3 = link copset deriv2 cox2   -- substitute free variables
       reduce cox3                          -- beta reduction
    where
      pos = position he

-- beta reduction, i.e., process CoxBlank and CoxForm1.
reduce :: forall c. (B.Write c) => C.Cox c -> B.Ab (Beta c)
reduce = red [] where
    red :: [C.NamedCox c] -> C.Cox c -> B.Ab (Beta c)
    red args cox = case cox of
        C.CoxLit    cp c        -> Right $ BetaLit  cp c
        C.CoxTerm   cp n i      -> Right $ BetaTerm cp n i
        C.CoxLocal  cp v k      -> Msg.abCoxReduce cp $ red args =<< kth v k args
        C.CoxFill   cp fn xs    -> Msg.abCoxReduce cp $ fill args fn $ substL args xs
        C.CoxCalc   cp _ _      -> Msg.abCoxReduce cp $ fill args cox []
        C.CoxWith   cp arg2 e   -> Msg.abCoxReduce cp $ red (arg2 ++ args) e
        C.CoxForm1  cp _ v _    -> Msg.abCoxReduce cp $ Msg.lackArg v
        C.CoxForm   cp _ _ _    -> Msg.abCoxReduce cp $ Msg.adlib "CoxForm"
        C.CoxBlank  cp v        -> Msg.abCoxReduce cp $ Msg.unkGlobalVar $ B.name v

    fill :: [C.NamedCox c] -> C.Cox c -> [B.Ab (C.Cox c)] -> B.Ab (Beta c)
    fill args f1 []              = red args f1
    fill args f1 xxs@(x:xs)      = case f1 of
        C.CoxForm1  cp _ v f2   -> Msg.abCoxFill cp $ do
                                       x' <- x
                                       let vx = (v, C.CoxWith cp args x')
                                       fill (vx : args) f2 xs
        C.CoxLocal  cp v k      -> Msg.abCoxFill cp $ do
                                       fn' <- kth v k args
                                       fill args fn' xxs
        C.CoxCalc   cp n f2     -> Msg.abCoxFill cp $ do
                                       xxs2 <- mapM id xxs
                                       let xxs3 = red args `map` xxs2
                                       Right $ BetaCall cp n f2 xxs3
        C.CoxFill   cp f2 xs2   -> Msg.abCoxFill cp $ fill args f2 $ substL args xs2 ++ xxs
        C.CoxWith   cp arg2 e2  -> Msg.abCoxFill cp $ fill (arg2 ++ args) e2 xxs
        _                       -> Msg.unkShow f1

    substL :: [C.NamedCox c] -> [C.Cox c] -> [B.Ab (C.Cox c)]
    substL = map . subst

    subst :: [C.NamedCox c] -> B.AbMap (C.Cox c)
    subst args (C.CoxLocal _ v k)    = kth v k args
    subst args (C.CoxFill cp f xs)   = do f' <- subst args f
                                          xs' <- subst args `mapM` xs
                                          Right $ C.CoxFill cp f' xs'
    subst _ x                        = Right x

    kth :: String -> Int -> [C.NamedCox c] -> B.Ab (C.Cox c)
    kth v 0  _    = Msg.unkGlobalVar v
    kth v k0 args = loop k0 args where
      loop 1 ((v2, x) : _)
          | v == v2           =  Right x
          | otherwise         =  Msg.unmatchBlank v k0 v2 vs
      loop k (_ : xs)         =  loop (k - 1) xs
      loop _ []               =  Msg.unkRefVar (v, k0) vs

      vs = map fst args

link :: forall c. C.CopSet c -> [C.NamedCox c] -> B.Map (C.Cox c)
link copset deriv = li where
    li cox@(C.CoxBlank _ n)   =  B.fromMaybe cox $ find n
    li cox                    =  C.coxCall cox li

    find n    = lookup n fs B.<|> findCont n
    findCont  = C.copsetFindCalc copset

    fs :: [(B.BlankName, C.Cox c)]
    fs = map (fmap li . normal) deriv

    normal (n, cop) = (B.BlankNormal n, cop)

-- put term positions for actural heading
position :: B.Head -> C.Cox c -> B.Ab (C.Cox c)
position he = spos where
    spos e = Msg.abCoxPosition [e] $ pos e
    pos (C.CoxTerm cp ns _) =
        let index = B.headIndex1 he ns
        in if all (>= 0) index
           then Right $ C.CoxTerm cp ns index
           else Msg.unkTerm [B.showNestedTermName ns] he
    pos (C.CoxFill cp  f xs)    = do f'  <- spos f
                                     xs' <- mapM spos xs
                                     Right $ C.CoxFill cp f' xs'
    pos (C.CoxForm1 cp tag v e) = do e' <- spos e
                                     Right $ C.CoxForm1 cp tag v e'
    pos e = Right e


-- --------------------------------------------  Run

type RunCox  c = C.Cox c -> B.Ab c
type RunList c = [c]     -> B.Ab c

coxRunCox :: (B.Write c, C.CRel c, C.CList c) =>
    C.CopSet c -> B.Head -> [c] -> RunCox c
coxRunCox cops he cs cox = coxRunList cops he cox cs

coxRunPure :: (B.Write c, C.CRel c, C.CList c) => C.CopSet c -> RunCox c
coxRunPure cops cox = coxRunList cops B.mempty cox []

coxRunList :: (B.Write c, C.CRel c, C.CList c) =>
    C.CopSet c -> B.Head -> C.Cox c -> RunList c
coxRunList cops he cox cs = coxRun cs =<< beta cops he cox

calcContent :: (C.CContent c) => C.CopSet c -> C.ContentCalc c
calcContent cops = calc where
    calc tree = coxRunPure cops =<< C.coxBuild calc cops tree

-- | Calculate content expression.
coxRun
  :: forall c. (C.CRel c, C.CList c, B.Write c)
  => [c]           -- ^ Tuple in body of relation
  -> Beta c        -- ^ Content expression
  -> B.Ab c        -- ^ Calculated literal content
coxRun args = run 0 where
    run :: Int -> Beta c -> B.Ab c
    run 1000 _ = B.bug "Too deep expression"
    run lv cox =
        let run' = run $ lv + 1
        in Msg.abCoxCalc [cox] $ case cox of
             BetaLit  _ c       ->  Right c
             BetaTerm _ _ [p]   ->  Right $ args !!! p
             BetaTerm _ _ ps    ->  term ps args
             BetaCall _ _ f xs  ->  f . map run' =<< sequence xs

    term :: [Int] -> [c] -> B.Ab c
    term []       _ = Msg.notFound "empty term"
    term (-1 : _) _ = Msg.notFound "negative term"
    term (p : ps) args2 =
        let c = args2 !!! p
        in if C.isRel c
           then rel ps $ C.gRel c
           else Right c

    rel :: [Int] -> B.Rel c -> B.Ab c
    rel ps (B.Rel _ args2) =
        C.putList =<< mapM (term ps) args2

(!!!) :: (B.Write a) =>[a] -> Int -> a
list !!! index = loop index list where
    loop 0 (x : _)  = x
    loop i (_ : xs) = loop (i - 1) xs
    loop _ _        = error $ show (B.doch list)
                              ++ " !!! " ++ show index


-- --------------------------------------------  getArgN

getArg1 :: [B.Ab c] -> B.Ab (B.Ab c)
getArg1 [x]       = Right x
getArg1 _         = Msg.unmatchType ""

getArg2 :: [B.Ab c] -> B.Ab (B.Ab c, B.Ab c)
getArg2 [x, y]    = Right (x, y)
getArg2 _         = Msg.unmatchType ""

getArg3 :: [B.Ab c] -> B.Ab (B.Ab c, B.Ab c, B.Ab c)
getArg3 [x, y, z] = Right (x, y, z)
getArg3 _         = Msg.unmatchType ""


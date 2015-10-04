{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content calcutation.

module Koshucode.Baala.Data.Church.Run
  ( RunCox, RunList,
    coxRunCox, coxRunList,
    calcContent,
  
    -- * Getting arguments
    getArg1, getArg2, getArg3,
    getRightArg1, getRightArg2, getRightArg3,
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Data.Token             as D
import qualified Koshucode.Baala.Data.Type              as D
import qualified Koshucode.Baala.Data.Content           as D
import qualified Koshucode.Baala.Data.Church.Build      as D
import qualified Koshucode.Baala.Data.Church.Cop        as D
import qualified Koshucode.Baala.Data.Church.Cox        as D
import qualified Koshucode.Baala.Base.Message           as Msg
import qualified Koshucode.Baala.Data.Content.Message   as Msg
import qualified Koshucode.Baala.Data.Church.Message    as Msg


-- --------------------------------------------  Beta reduction

data Beta c
    = BetaLit  [B.CodePt] c                     -- ^ Literal content
    | BetaTerm [B.CodePt] [D.TermName] [Int]    -- ^ Term reference, its name and position
    | BetaCall [B.CodePt] D.BlankName (D.CopCalc c) [B.Ab (Beta c)]  -- ^ Function application

instance B.CodePtr (Beta c) where
    codePtList (BetaLit  cp _)      = cp
    codePtList (BetaTerm cp _ _)    = cp
    codePtList (BetaCall cp _ _ _)  = cp

-- | Reduce content expression.
beta :: (B.Write c) => D.CopSet c -> D.Head -> D.Cox c -> B.Ab (Beta c)
beta copset he cox =
    do let deriv = D.copsetDerived copset
       deriv2  <- B.sequenceSnd $ B.mapSndTo pos deriv
       cox2    <- pos cox                   -- put term index
       let cox3 = link copset deriv2 cox2   -- substitute free variables
       reduce cox3                          -- beta reduction
    where
      pos = position he

-- beta reduction, i.e., process CoxBlank and CoxForm1.
reduce :: forall c. (B.Write c) => D.Cox c -> B.Ab (Beta c)
reduce = red [] where
    red :: [D.NamedCox c] -> D.Cox c -> B.Ab (Beta c)
    red args cox = case cox of
        D.CoxLit    cp c        -> Right $ BetaLit  cp c
        D.CoxTerm   cp n i      -> Right $ BetaTerm cp n i
        D.CoxLocal  cp v k      -> Msg.abCoxReduce cp $ red args =<< kth v k args
        D.CoxFill   cp fn xs    -> Msg.abCoxReduce cp $ fill args fn $ substL args xs
        D.CoxCalc   cp _ _      -> Msg.abCoxReduce cp $ fill args cox []
        D.CoxWith   cp arg2 e   -> Msg.abCoxReduce cp $ red (arg2 ++ args) e
        D.CoxForm1  cp _ v _    -> Msg.abCoxReduce cp $ Msg.lackArg v
        D.CoxForm   cp _ _ _    -> Msg.abCoxReduce cp $ Msg.adlib "CoxForm"
        D.CoxBlank  cp v        -> Msg.abCoxReduce cp $ Msg.unkGlobalVar $ B.name v

    fill :: [D.NamedCox c] -> D.Cox c -> [B.Ab (D.Cox c)] -> B.Ab (Beta c)
    fill args f1 []              = red args f1
    fill args f1 xxs@(x:xs)      = Msg.abCoxFill (B.codePtList f1) $ case f1 of
        D.CoxForm1  cp _ v f2   -> do x' <- x
                                      let vx = (v, D.CoxWith cp args x')
                                      fill (vx : args) f2 xs
        D.CoxLocal  _ v k       -> do fn' <- kth v k args
                                      fill args fn' xxs
        D.CoxCalc   cp n f2     -> do xxs2 <- mapM id xxs
                                      let xxs3 = red args `map` xxs2
                                      Right $ BetaCall cp n f2 xxs3
        D.CoxFill   _ f2 xs2    -> fill args f2 $ substL args xs2 ++ xxs
        D.CoxWith   _ arg2 e2   -> fill (arg2 ++ args) e2 xxs
        D.CoxBlank  _ n         -> Msg.unkCop $ B.name n
        _                       -> Msg.unkShow f1

    substL :: [D.NamedCox c] -> [D.Cox c] -> [B.Ab (D.Cox c)]
    substL = map . subst

    subst :: [D.NamedCox c] -> B.AbMap (D.Cox c)
    subst args (D.CoxLocal _ v k)    = kth v k args
    subst args (D.CoxFill cp f xs)   = do f' <- subst args f
                                          xs' <- subst args `mapM` xs
                                          Right $ D.CoxFill cp f' xs'
    subst _ x                        = Right x

    kth :: String -> Int -> [D.NamedCox c] -> B.Ab (D.Cox c)
    kth v 0  _    = Msg.unkGlobalVar v
    kth v k0 args = loop k0 args where
      loop 1 ((v2, x) : _)
          | v == v2           =  Right x
          | otherwise         =  Msg.unmatchBlank v k0 v2 vs
      loop k (_ : xs)         =  loop (k - 1) xs
      loop _ []               =  Msg.unkRefVar (v, k0) vs

      vs = map fst args

link :: forall c. D.CopSet c -> [D.NamedCox c] -> B.Map (D.Cox c)
link copset deriv = li where
    li cox@(D.CoxBlank _ n)   =  B.fromMaybe cox $ find n
    li cox                    =  D.coxCall cox li

    find n    = lookup n fs B.<|> findCont n
    findCont  = D.copsetFindCalc copset

    fs :: [(D.BlankName, D.Cox c)]
    fs = map (fmap li . normal) deriv

    normal (n, cop) = (D.BlankNormal n, cop)

-- put term positions for actural heading
position :: D.Head -> D.Cox c -> B.Ab (D.Cox c)
position he = spos where
    spos e = Msg.abCoxPosition [e] $ pos e
    pos (D.CoxTerm cp ns _) =
        let index = D.headIndex1 he ns
        in if all (>= 0) index
           then Right $ D.CoxTerm cp ns index
           else Msg.unkTerm [D.showTermPath ns] he
    pos (D.CoxFill cp  f xs)    = do f'  <- spos f
                                     xs' <- mapM spos xs
                                     Right $ D.CoxFill cp f' xs'
    pos (D.CoxForm1 cp tag v e) = do e' <- spos e
                                     Right $ D.CoxForm1 cp tag v e'
    pos e = Right e


-- --------------------------------------------  Run

type RunCox  c = D.Cox c -> B.Ab c
type RunList c = [c]     -> B.Ab c

coxRunCox :: (B.Write c, D.CRel c, D.CList c) =>
    D.CopSet c -> D.Head -> [c] -> RunCox c
coxRunCox cops he cs cox = coxRunList cops he cox cs

coxRunPure :: (B.Write c, D.CRel c, D.CList c) => D.CopSet c -> RunCox c
coxRunPure cops cox = coxRunList cops B.mempty cox []

coxRunList :: (B.Write c, D.CRel c, D.CList c) =>
    D.CopSet c -> D.Head -> D.Cox c -> RunList c
coxRunList cops he cox cs = coxRun cs =<< beta cops he cox

calcContent :: (D.CContent c) => D.CopSet c -> D.ContentCalc c
calcContent cops = calc where
    calc tree = coxRunPure cops =<< D.coxBuild calc cops tree

-- | Calculate content expression.
coxRun
  :: forall c. (D.CRel c, D.CList c, B.Write c)
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
        in if D.isRel c
           then rel ps $ D.gRel c
           else Right c

    rel :: [Int] -> D.Rel c -> B.Ab c
    rel ps (D.Rel _ args2) =
        D.putList =<< mapM (term ps) args2

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

getRightArg1 :: [B.Ab c] -> B.Ab c
getRightArg1 = getArg1 B.>=> id

getRightArg2 :: [B.Ab c] -> B.Ab (c, c)
getRightArg2 = getArg2 B.>=> B.right2

getRightArg3 :: [B.Ab c] -> B.Ab (c, c, c)
getRightArg3 = getArg3 B.>=> B.right3


{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content calcutation.

module Koshucode.Baala.Core.Content.Run
( RunCox, RunList,
  coxRunCox, coxRunList,
  getArg1, getArg2, getArg3,
) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Content.Class     as C
import qualified Koshucode.Baala.Core.Content.Cox       as C
import qualified Koshucode.Baala.Core.Message           as Message


-- --------------------------------------------  Beta reduction

data Beta c
    = BetaLit  [B.CodePoint] c                     -- ^ Literal content
    | BetaTerm [B.CodePoint] [B.TermName] [Int]    -- ^ Term reference, its name and position
    | BetaCall [B.CodePoint] B.BlankName (C.CopFun c) [B.Ab (Beta c)]  -- ^ Function application

instance B.CodePointer (Beta c) where
    codePoints (BetaLit  cp _)      =  cp
    codePoints (BetaTerm cp _ _)    =  cp
    codePoints (BetaCall cp _ _ _)  =  cp

-- | Reduce content expression.
beta :: (B.Write c) => C.CopBundle c -> B.Relhead -> C.Cox c -> B.Ab (Beta c)
beta (base, deriv) he cox =
    do deriv2  <- B.sequenceSnd $ B.mapSndTo pos deriv
       cox2    <- pos cox                      -- put term index
       let cox3 = link (base, deriv2) cox2     -- substitute free variables
       reduce cox3                             -- beta reduction
    where
      pos = position he

-- beta reduction, i.e., process CoxBlank and CoxForm1.
reduce :: forall c. (B.Write c) => C.Cox c -> B.Ab (Beta c)
reduce = red [] where
    a1 = B.abortable "cox-reduce"
    a2 = B.abortable "cox-refill"

    red :: [C.NamedCox c] -> C.Cox c -> B.Ab (Beta c)
    red args cox = case cox of
        C.CoxLit    cp c        ->  Right $ BetaLit  cp c
        C.CoxTerm   cp n i      ->  Right $ BetaTerm cp n i
        C.CoxLocal  cp v k      ->  a1 cp $ red args =<< kth v k args
        C.CoxRefill cp fn xs    ->  a1 cp $ refill args fn $ substL args xs
        C.CoxBase   cp _ _      ->  a1 cp $ refill args cox []
        C.CoxWith   cp arg2 e   ->  a1 cp $ red (arg2 ++ args) e
        C.CoxForm1  cp _ v _    ->  a1 cp $ Message.lackArg v
        C.CoxForm   cp _ _ _    ->  a1 cp $ Message.adlib "CoxForm"
        C.CoxBlank  cp v        ->  a1 cp $ Message.unkGlobalVar $ B.name v

    refill :: [C.NamedCox c] -> C.Cox c -> [B.Ab (C.Cox c)] -> B.Ab (Beta c)
    refill args f1 []           =   red args f1
    refill args f1 xxs@(x:xs)   =   case f1 of
        C.CoxForm1  cp _ v f2   ->  a2 cp $ do x' <- x
                                               let vx = (v, C.CoxWith cp args x')
                                               refill (vx : args) f2 xs
        C.CoxLocal  cp v k      ->  a2 cp $ do fn' <- kth v k args
                                               refill args fn' xxs
        C.CoxBase   cp n f2     ->  a2 cp $ do xxs2 <- mapM id xxs
                                               let xxs3 = red args `map` xxs2
                                               Right $ BetaCall cp n f2 xxs3
        C.CoxRefill cp f2 xs2   ->  a2 cp $ refill args f2 $ substL args xs2 ++ xxs
        C.CoxWith   cp arg2 e2  ->  a2 cp $ refill (arg2 ++ args) e2 xxs
        _                       ->  Message.unkShow f1

    substL :: [C.NamedCox c] -> [C.Cox c] -> [B.Ab (C.Cox c)]
    substL = map . subst

    subst :: [C.NamedCox c] -> B.AbMap (C.Cox c)
    subst args (C.CoxLocal _ v k)    = kth v k args
    subst args (C.CoxRefill cp f xs) = do f' <- subst args f
                                          xs' <- subst args `mapM` xs
                                          Right $ C.CoxRefill cp f' xs'
    subst _ x                        = Right x

    kth :: String -> Int -> [C.NamedCox c] -> B.Ab (C.Cox c)
    kth v 0  _    = Message.unkGlobalVar v
    kth v k0 args = loop k0 args where
      loop 1 ((v2, x) : _)
          | v == v2           =  Right x
          | otherwise         =  Message.unmatchBlank v k0 v2 vs
      loop k (_ : xs)         =  loop (k - 1) xs
      loop _ []               =  Message.unkRefVar (v, k0) vs

      vs = map fst args

link :: forall c. C.CopBundle c -> B.Map (C.Cox c)
link (base, deriv) = li where
    li cox@(C.CoxBlank _ n)   =  B.fromMaybe cox $ lookup n fs
    li cox                    =  C.coxCall cox li

    fs :: [C.CoxAssn c]
    fs = map (fmap li . assn) deriv ++ map named base

    assn (n, cop) = (B.BlankNormal n, cop)

    named :: C.Cop c -> C.CoxAssn c
    named (C.CopFun  n f) = (n, C.CoxBase [] n f)
    named (C.CopCox  n _) = (n, C.CoxBase [] n undefined)
    named (C.CopTree n _) = (n, C.CoxBase [] n undefined)

-- put term positions for actural heading
position :: B.Relhead -> C.Cox c -> B.Ab (C.Cox c)
position he = spos where
    spos e = B.abortable "cox-position" [e] $ pos e
    pos (C.CoxTerm cp ns _) =
        let index = B.headIndex1 he ns
        in if all (>= 0) index
           then Right $ C.CoxTerm cp ns index
           else Message.unkTerm [B.showNestedTermName ns] he
    pos (C.CoxRefill cp  f xs)  = do f'  <- spos f
                                     xs' <- mapM spos xs
                                     Right $ C.CoxRefill cp f' xs'
    pos (C.CoxForm1 cp tag v e) = do e' <- spos e
                                     Right $ C.CoxForm1 cp tag v e'
    pos e = Right e


-- --------------------------------------------  Run

type RunCox  c = C.Cox c -> B.Ab c
type RunList c = [c]     -> B.Ab c

coxRunCox :: (B.Write c, C.CRel c, C.CList c) =>
    C.CopBundle c -> B.Relhead -> [c] -> RunCox c
coxRunCox cops he cs cox = coxRunList cops he cox cs

coxRunList :: (B.Write c, C.CRel c, C.CList c) =>
    C.CopBundle c -> B.Relhead -> C.Cox c -> RunList c
coxRunList cops he cox cs = coxRun cs =<< beta cops he cox

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
        in B.abortable "cox-calc" [cox] $ case cox of
             BetaLit  _ c       ->  Right c
             BetaTerm _ _ [p]   ->  Right $ args !!! p
             BetaTerm _ _ ps    ->  term ps args
             BetaCall _ _ f xs  ->  f . map run' =<< sequence xs

    term :: [Int] -> [c] -> B.Ab c
    term []       _ = Message.notFound "empty term"
    term (-1 : _) _ = Message.notFound "negative term"
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
getArg1 _         = Message.unmatchType ""

getArg2 :: [B.Ab c] -> B.Ab (B.Ab c, B.Ab c)
getArg2 [x, y]    = Right (x, y)
getArg2 _         = Message.unmatchType ""

getArg3 :: [B.Ab c] -> B.Ab (B.Ab c, B.Ab c, B.Ab c)
getArg3 [x, y, z] = Right (x, y, z)
getArg3 _         = Message.unmatchType ""


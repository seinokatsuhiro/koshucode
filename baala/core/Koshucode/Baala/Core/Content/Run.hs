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
    | BetaCall [B.CodePoint] String (C.CopFun c) [B.Ab (Beta c)]  -- ^ Function application

instance B.CodePointer (Beta c) where
    codePoint (BetaLit  cp _)      =  cp
    codePoint (BetaTerm cp _ _)    =  cp
    codePoint (BetaCall cp _ _ _)  =  cp

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
    ab1 = B.abortable "cox-reduce"
    ab2 = B.abortable "cox-apply"

    red :: [B.Named (C.Cox c)] -> C.Cox c -> B.Ab (Beta c)
    red args cox = case cox of
        C.CoxLit    cp c       ->  Right $ BetaLit  cp c
        C.CoxTerm   cp n i     ->  Right $ BetaTerm cp n i
        C.CoxBlank  cp v k     ->  ab1 cp $ red args =<< kth v k args
        C.CoxRefill cp fn xs   ->  ab1 cp $ join args fn $ fills args xs
        C.CoxBase   cp _ _     ->  ab1 cp $ join args cox []
        C.CoxForm1  cp _ v _   ->  ab1 cp $ Message.lackArg v
        C.CoxForm   cp _ _ _   ->  ab1 cp $ Message.adlib "CoxForm"

    join :: [B.Named (C.Cox c)] -> C.Cox c -> [B.Ab (C.Cox c)] -> B.Ab (Beta c)
    join args fn []             =  red args fn
    join args fn xxs@(x:xs)     =  case fn of
        C.CoxForm1 cp _ v f2   ->  ab2 cp $ do
                                       x' <- x
                                       join ((v, x') : args) f2 xs
        C.CoxBlank cp v k      ->  ab2 cp $ do
                                       fn' <- kth v k args
                                       join args fn' xxs
        C.CoxBase  cp n f      ->  ab2 cp $ do
                                       xxs2 <- mapM id xxs
                                       let xxs3 = red args `map` xxs2
                                       Right $ BetaCall cp n f xxs3
        C.CoxRefill cp f2 xs2  ->  ab2 cp $ join args f2 $ fills args xs2 ++ xxs
        _                      ->  Message.unkShow fn

    fills :: [B.Named (C.Cox c)] -> [C.Cox c] -> [B.Ab (C.Cox c)]
    fills = map . fill

    fill :: [B.Named (C.Cox c)] -> B.AbMap (C.Cox c)
    fill args (C.CoxBlank _ v k)     = kth v k args
    fill args (C.CoxRefill cp fn xs) = do fn' <- fill args fn
                                          xs' <- fill args `mapM` xs
                                          Right $ C.CoxRefill cp fn' xs'
    fill _ x                         = Right x

    kth :: String -> Int -> [B.Named (C.Cox c)] -> B.Ab (C.Cox c)
    kth v 0  _    = Message.unkGlobalVar v
    kth v k0 args = loop k0 args where
      loop 1 ((v2, x) : _)
          | v == v2           =  Right x
          | otherwise         =  Message.unmatchVar v k0 v2
      loop k (_ : xs)         =  loop (k - 1) xs
      loop _ []               =  Message.unkRefVar v k0

link :: forall c. C.CopBundle c -> B.Map (C.Cox c)
link (base, deriv) = li where
    li cox =
        case cox of
          C.CoxBlank _ n 0 -> B.fromMaybe cox $ lookup n fs
          _                -> C.mapToCox li cox

    fs :: [C.NamedCox c]
    fs = map (fmap li) deriv ++ map namedBase base

    namedBase :: C.Cop c -> C.NamedCox c
    namedBase (C.CopFun  n f) = (n, C.CoxBase [] n f)
    namedBase (C.CopCox  n _) = (n, C.CoxBase [] n undefined)
    namedBase (C.CopTree n _) = (n, C.CoxBase [] n undefined)

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


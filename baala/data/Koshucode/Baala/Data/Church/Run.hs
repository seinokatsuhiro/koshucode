{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content calculation.

module Koshucode.Baala.Data.Church.Run
  ( CalcTuple, calcTuple,
    calcCox, calcTree,

    -- * Getting arguments
    getRightArg1, getRightArg2, getRightArg3,
  ) where

import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax                 as S
import qualified Koshucode.Baala.Type                   as T
import qualified Koshucode.Baala.Data.Class             as D
import qualified Koshucode.Baala.Data.Decode            as D
import qualified Koshucode.Baala.Data.Church.Build      as D
import qualified Koshucode.Baala.Data.Church.Cop        as D
import qualified Koshucode.Baala.Data.Church.Cox        as D
import qualified Koshucode.Baala.Base.Message           as Msg
import qualified Koshucode.Baala.Data.Class.Message     as Msg
import qualified Koshucode.Baala.Data.Church.Message    as Msg


-- --------------------------------------------  Beta reduction

data Beta c
    = BetaLit  [B.CodePos] c
      -- ^ Literal content
    | BetaTerm [B.CodePos] [S.TermName] [S.TermIndex]
      -- ^ Term reference, its name and position
    | BetaCall [B.CodePos] S.BlankName (D.CopCalc c) [B.Ab (Beta c)]
      -- ^ Function application

instance (B.MixEncode c) => Show (Beta c) where
    show (BetaLit  cp c)     = "BetaLit "  ++ show cp ++ " " ++ B.encode c
    show (BetaTerm cp ns _ ) = "BetaTerm " ++ show cp ++ " " ++ S.termPathString ns
    show (BetaCall cp n _ _) = "BetaCall " ++ show cp ++ " " ++ show n

instance B.GetCodePos (Beta c) where
    getCPs (BetaLit  cp _)      = cp
    getCPs (BetaTerm cp _ _)    = cp
    getCPs (BetaCall cp _ _ _)  = cp

-- | Reduce content expression.
beta :: (B.MixEncode c) => D.CopSet c -> T.Head -> D.Cox c -> B.Ab (Beta c)
beta copset he cox =
    do let deriv = D.copsetDerived copset
       deriv2  <- B.sequenceSnd (pos O.<$$> deriv)
       cox2    <- pos cox                   -- put term index
       let cox3 = link copset deriv2 cox2   -- substitute free variables
       reduce cox3                          -- beta reduction
    where
      pos = position he

-- beta reduction, i.e., process CoxBlank and CoxForm1.
reduce :: forall c. (B.MixEncode c) => D.Cox c -> B.Ab (Beta c)
reduce = red [] where
    red :: [D.NamedCox c] -> D.Cox c -> B.Ab (Beta c)
    red args cox = Msg.abCoxReduce cox $ case cox of
        D.CoxLit    cp c      -> Right $ BetaLit  cp c
        D.CoxTerm   cp n i    -> Right $ BetaTerm cp n i
        D.CoxLocal  _ v k     -> red args =<< kth v k args
        D.CoxFill   _ fn xs   -> fill args fn $ substL args xs
        D.CoxCalc   _ _ _     -> fill args cox []
        D.CoxWith   _ arg2 e  -> red (arg2 ++ args) e
        D.CoxForm1  _ _ v _   -> Msg.lackArg v
        D.CoxForm   _ _ _ _   -> Msg.adlib "CoxForm"
        D.CoxBlank  _ v       -> Msg.unkGlobalVar $ B.name v

    fill :: [D.NamedCox c] -> D.Cox c -> [D.AbCox c] -> B.Ab (Beta c)
    fill args (D.CoxCalc cp n f) xs =
                                   do xs2 <- sequence xs
                                      let xs3 = red args `map` xs2
                                      Right $ BetaCall cp n f xs3
    fill args f1 []              = red args f1
    fill args f1 xxs@(x:xs)      = Msg.abCoxFill (B.getCPs f1) $ case f1 of
        D.CoxForm1  cp _ v f2   -> do x' <- x
                                      let vx = (v, D.CoxWith cp args x')
                                      fill (vx : args) f2 xs
        D.CoxLocal  _ v k       -> do fn' <- kth v k args
                                      fill args fn' xxs
        D.CoxFill   _ f2 xs2    -> fill args f2 $ substL args xs2 ++ xxs
        D.CoxWith   _ arg2 e2   -> fill (arg2 ++ args) e2 xxs
        D.CoxBlank  _ n         -> Msg.unkCop $ B.name n
        _                       -> Msg.unkShow f1

    substL :: [D.NamedCox c] -> [D.Cox c] -> [D.AbCox c]
    substL = map . subst

    subst :: [D.NamedCox c] -> B.AbMap (D.Cox c)
    subst args (D.CoxLocal _ v k)    = kth v k args
    subst args (D.CoxFill cp f xs)   = do f'  <- subst args f
                                          xs' <- subst args `mapM` xs
                                          Right $ D.CoxFill cp f' xs'
    subst _ x                        = Right x

    kth :: String -> Int -> [D.NamedCox c] -> D.AbCox c
    kth v 0  _    = Msg.unkGlobalVar v
    kth v k0 args = loop k0 args where
      loop 1 ((v2, x) : _)
          | v == v2           =  Right x
          | otherwise         =  Msg.unmatchBlank v k0 v2 vs
      loop k (_ : xs)         =  loop (k - 1) xs
      loop _ []               =  Msg.unkRefVar (v, k0) vs

      vs = map fst args

link :: forall c. D.CopSet c -> [D.NamedCox c] -> O.Map (D.Cox c)
link copset deriv = li where
    li cox@(D.CoxBlank _ n)   =  putCp cox $ find n
    li cox                    =  D.coxCall cox li

    find n    = lookup n fs B.<|> findCont n
    findCont  = D.copsetFindCalc copset

    putCp cox (Just (D.CoxCalc [] n f))  = D.CoxCalc (B.getCPs cox) n f
    putCp _   (Just cox) = cox
    putCp cox (Nothing)  = cox

    fs :: [(S.BlankName, D.Cox c)]
    fs = map (fmap li . normal) deriv

    normal (n, cop) = (S.BlankNormal n, cop)

-- put term positions for actural heading
position :: T.Head -> D.Cox c -> D.AbCox c
position he = spos where
    spos e = Msg.abCoxPosition e $ pos e
    pos (D.CoxTerm cp ns _) =
        let index = T.headIndex1 he ns
        in if all (>= 0) index
           then Right $ D.CoxTerm cp ns index
           else Msg.unkTerm ns he  -- todo: term path
    pos (D.CoxFill cp f xs)     = do f'  <- spos f
                                     xs' <- mapM spos xs
                                     Right $ D.CoxFill cp f' xs'
    pos (D.CoxForm1 cp tag v e) = do e' <- spos e
                                     Right $ D.CoxForm1 cp tag v e'
    pos e = Right e


-- --------------------------------------------  Run

-- | [Tuple-to-Content]
--      Type for tuple-to-content calculation.
type CalcTuple c = [c] -> B.Ab c

-- | [Cox-to-Tuple-to-Content]
--     Create tuple calculator from content expression.
calcTuple :: (D.GetCops cops, D.CContent c) => cops c -> T.Head -> D.Cox c -> CalcTuple c
calcTuple cops he cox cs = coxRun cs =<< beta (D.getCops cops) he cox

-- | [Tuple-to-Cox-to-Content]
--     Calculate content expression with specific tuple.
calcCox :: (D.GetCops cops, D.CContent c) => cops c -> T.Head -> [c] -> D.Cox c -> B.Ab c
calcCox cops he cs cox = calcTuple (D.getCops cops) he cox cs

-- | [Tree-to-Content]
--     Calculate token tree as content expression.
calcTree :: (D.GetCops cops, D.CContent c) => cops c -> D.CalcContent c
calcTree cops tree =
    do let cops' = D.getCops cops
       cox <- D.treeCox cops' tree
       calcTuple cops' mempty cox []

-- | Calculate content expression.
coxRun
  :: forall c. (D.CContent c)
  => [c]           -- ^ Tuple in body of relation
  -> Beta c        -- ^ Content expression
  -> B.Ab c        -- ^ Calculated literal content
coxRun args = run 0 where
    run :: Int -> Beta c -> B.Ab c
    run 1000 _ = B.bug "Too deep expression"
    run lv cox =
        let run' = run $ lv + 1
        in Msg.abCoxCalc cox $ case cox of
             BetaLit  _ c       -> Right c
             BetaTerm _ _ [p]   -> Right $ args !!! p
             BetaTerm _ _ ps    -> term ps args
             BetaCall _ _ f xs  -> f . map run' =<< sequence xs

    term :: [S.TermIndex] -> [c] -> B.Ab c
    term []       _ = Msg.adlib "empty term"
    term (-1 : _) _ = Msg.adlib "negative term"
    term (p : ps) args2 =
        let c = args2 !!! p
        in if D.isRel c
           then rel ps $ D.gRel c
           else Right c

    rel :: [S.TermIndex] -> T.Rel c -> B.Ab c
    rel ps (T.Rel _ args2) =
        D.putList =<< mapM (term ps) args2

(!!!) :: (B.MixEncode c) => [c] -> S.TermIndex -> c
list !!! index = loop index list where
    loop 0 (x : _)  = x
    loop i (_ : xs) = loop (i - 1) xs
    loop _ _        = error $ (unwords $ map B.encode list)
                              ++ " !!! " ++ show index


-- --------------------------------------------  getArgN

-- | Extract single non-abortable argument.
getRightArg1 :: (D.Basis c) => [B.Ab c] -> B.Ab c
getRightArg1 [Right x] = Right x
getRightArg1 cs = Msg.badArg cs

-- | Extract two non-abortable arguments.
getRightArg2 :: (D.Basis c) => [B.Ab c] -> B.Ab (c, c)
getRightArg2 [Right x, Right y] = Right (x, y)
getRightArg2 cs = Msg.badArg cs

-- | Extract three non-abortable arguments.
getRightArg3 :: (D.Basis c) => [B.Ab c] -> B.Ab (c, c, c)
getRightArg3 [Right x, Right y, Right z] = Right (x, y, z)
getRightArg3 cs = Msg.badArg cs


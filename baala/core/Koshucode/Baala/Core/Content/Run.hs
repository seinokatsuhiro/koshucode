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


data Beta c
    = BetaLit    [B.CodePoint] c                   -- ^ Literal content
    | BetaTerm   [B.CodePoint] [B.TermName] [Int]  -- ^ Term reference, its name and position
    | BetaBase   [B.CodePoint] String (C.CopFun c) -- ^ Base function
    | BetaApplyL [B.CodePoint] (Beta c) [Beta c]   -- ^ Function application (multiple arguments)

instance B.CodePointer (Beta c) where
    codePoint (BetaLit    pt _)   = pt
    codePoint (BetaTerm   pt _ _) = pt
    codePoint (BetaBase   pt _ _) = pt
    codePoint (BetaApplyL pt _ _) = pt

instance (B.Write c) => Show (Beta c) where
    show = show . B.doc

instance (B.Write c) => B.Write (Beta c) where
    write = docBeta

docBeta :: (B.Write c) => B.StringMap -> Beta c -> B.Doc
docBeta sh = d (0 :: Int) where
    d 10 _ = B.write sh "..."
    d n e =
        case e of
          BetaLit    _ c      -> B.write  sh c
          BetaTerm   _ ns _   -> B.writeH sh $ map ('/':) ns
          BetaBase   _ name _ -> B.write  sh name
          BetaApplyL _ f xs   -> let f'  = B.write sh "ap" B.<+> d' f
                                     xs' = B.nest 3 $ B.writeV sh (map d' xs)
                                 in f' B.$$ xs'
        where d' = d $ n + 1

-- | Reduce content expression.
beta :: (B.Write c) => C.CopBundle c -> B.Relhead -> C.Cox c -> B.Ab (Beta c)
beta cops h =
    Right . reduce      -- beta reduction
          . link cops   -- substitute free variables
          B.<=< position h

-- beta reduction, i.e., process CoxVar and CoxDeriv.
reduce :: forall c. (B.Write c) => C.Cox c -> Beta c
reduce = red [] where
    red :: [Beta c] -> C.Cox c -> Beta c
    red args cox =
        case cox of
          C.CoxLit    src c       ->  BetaLit  src c
          C.CoxTerm   src n i     ->  BetaTerm src n i
          C.CoxBase   src n f     ->  BetaBase src n f
          C.CoxVar    _ _ 0       ->  B.bug "global variable"
          C.CoxVar    _ _ i       ->  args !!! (i - 1)
          C.CoxApplyL src fn xs   ->  app args src fn xs
          C.CoxDeriv  _ _ e       ->  red args e
          C.CoxDerivL _ _ _       ->  B.bug "CoxDerivL"

    app args _ fn [] = red args fn
    app args src fn xxs@(x:xs) =
        let xxs' = red args `map` xxs
        in case fn of
             C.CoxBase src2 n f   ->  BetaApplyL src (BetaBase src2 n f) xxs'
             C.CoxDeriv _ _ e     ->  let args' = red args x : args
                                      in red args' $ C.CoxApplyL src e xs
             _                    ->  let fn' = red args fn
                                      in BetaApplyL src fn' xxs'

link :: forall c. C.CopBundle c -> B.Map (C.Cox c)
link (base, deriv) = li where
    li cox =
        case cox of
          C.CoxVar _ n 0 -> B.fromMaybe cox $ lookup n fs
          _              -> C.mapToCox li cox

    fs :: [C.NamedCox c]
    fs = map (fmap li) deriv ++ map namedBase base

    namedBase :: C.Cop c -> C.NamedCox c
    namedBase (C.CopFun n f) = (n, C.CoxBase [] n f)
    namedBase (C.CopCox n _) = (n, C.CoxBase [] n undefined)
    namedBase (C.CopSyn n _) = (n, C.CoxBase [] n undefined)

-- put term positions for actural heading
position :: B.Relhead -> C.Cox c -> B.Ab (C.Cox c)
position he = spos where
    spos e = B.abortable "position" [e] $ pos e
    pos (C.CoxTerm src ns _) =
        let index = B.headIndex1 he ns
        in if all (>= 0) index
           then Right $ C.CoxTerm src ns index
           else Message.unkTerm [B.showNestedTermName ns] he
    pos (C.CoxApplyL src  f xs) = do f'  <- spos f
                                     xs' <- mapM spos xs
                                     Right $ C.CoxApplyL src f' xs'
    pos (C.CoxDeriv src  v e)   = do e' <- spos e
                                     Right $ C.CoxDeriv src v e'
    pos e = Right e


-- ----------------------  Run

getArg1 :: [B.Ab c] -> B.Ab (B.Ab c)
getArg1 [x]       = Right x
getArg1 _         = Message.unmatchType ""

getArg2 :: [B.Ab c] -> B.Ab (B.Ab c, B.Ab c)
getArg2 [x, y]    = Right (x, y)
getArg2 _         = Message.unmatchType ""

getArg3 :: [B.Ab c] -> B.Ab (B.Ab c, B.Ab c, B.Ab c)
getArg3 [x, y, z] = Right (x, y, z)
getArg3 _         = Message.unmatchType ""

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
        in B.abortable "calc" [cox] $ case cox of
             BetaLit    _ c       -> Right c
             BetaTerm   _ _ [p]   -> Right $ args !!! p
             BetaTerm   _ _ ps    -> term ps args
             BetaApplyL _ e []    -> run' e
             BetaApplyL _ (BetaBase _ _ f) xs -> f $ map run' xs
             _ -> Message.adlibs $ "Unknown cox" : (lines $ show cox)

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


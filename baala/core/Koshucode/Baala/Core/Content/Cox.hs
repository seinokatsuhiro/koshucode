{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content calcutation.

module Koshucode.Baala.Core.Content.Cox
( -- $Process

  -- * Expression
  Cox, CoxCore (..),

  -- * Operator
  Cop (..), CopFun, CopSyn,
  isCopFunction,
  isCopSyntax,

  -- * Construction
  NamedCox,
  coxAlpha, coxBeta,
  checkIrreducible,

  -- * Run
  coxRun,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content.Literal as C


-- ----------------------  expressions and operators

-- | Content expressions with source code.
type Cox c = B.Sourced (CoxCore c)

type NamedCox c = B.Named (Cox c)

-- | Content expressions.
data CoxCore c
    = CoxLit c                     -- ^ A:   Literal content
    | CoxTerm [B.Termname] [Int]   -- ^ A:   Term reference, its name and position
    | CoxBase String (CopFun c)    -- ^ A:   Base function

    | CoxVar String Int            -- ^ B:   Variable, its name and De Bruijn index
    | CoxApplyL  (Cox c) [Cox c]   -- ^ A/B: Function application (multiple arguments)
    | CoxDeriv   String  (Cox c)   -- ^ B:   Derived function

    | CoxDerivL [String] (Cox c)   -- ^ C:   Derived function (multiple variables)

instance (B.Pretty c) => Show (CoxCore c) where
    show = show . B.doc

instance (B.Pretty c) => B.Pretty (CoxCore c) where
    doc = docCore

docCore :: (B.Pretty c) => CoxCore c -> B.Doc
docCore = d (0 :: Int) where
    d 10 _ = B.doc "..."
    d n e =
        case e of
          CoxLit c       -> B.doc c
          CoxTerm ns _   -> B.doch ns
          CoxBase name _ -> B.doc name
          CoxVar v i     -> B.doc v B.<> B.doc "/" B.<> B.doc i
          CoxApplyL (B.Sourced _ f) xs ->
              (B.doc "ap" B.<+> d' f) B.$$ (B.nest 3 $ B.docv (map (d' . B.unsourced) xs))
          CoxDeriv  v  (B.Sourced _ b) -> fn (B.doc v)   (d' b)
          CoxDerivL vs (B.Sourced _ b) -> fn (B.doch vs) (d' b)
        where
          d' = d $ n + 1
          fn v b = B.docWraps "(|" "|)" $ v B.<+> B.doc "|" B.<+> b

mapToCox :: B.Map (Cox c) -> B.Map (CoxCore c)
mapToCox g (CoxApplyL f xs) = CoxApplyL (g f) (map g xs)
mapToCox g (CoxDeriv v b)   = CoxDeriv v (g b)
mapToCox g (CoxDerivL vs b) = CoxDerivL vs (g b)
mapToCox _ e = e

-- | Term-content operator.
data Cop c
    = CopFun String (CopFun c)  -- ^ Function
    | CopSyn String (CopSyn)    -- ^ Syntax

type CopFun c = [B.Ab c] -> B.Ab c
type CopSyn   = [B.TokenTree] -> B.Ab B.TokenTree

instance Show (Cop c) where
    show (CopFun n _) = "(CopFun " ++ show n ++ " _)"
    show (CopSyn n _) = "(CopSyn " ++ show n ++ " _)"

instance B.Name (Cop c) where
    name (CopFun n _) = n
    name (CopSyn n _) = n

isCopFunction :: Cop c -> Bool
isCopFunction (CopFun _ _) = True
isCopFunction _            = False

isCopSyntax :: Cop c -> Bool
isCopSyntax (CopSyn _ _) = True
isCopSyntax _            = False



-- ----------------------  alpha construction

coxAlpha :: forall c. (C.CContent c)
  => ([Cop c], [B.Named B.InfixHeight]) -> B.TokenTree -> B.Ab (Cox c)
coxAlpha (syn, htab) = body where
    body :: B.TokenTree -> B.Ab (Cox c)
    body tree =
        Right . debruijn         -- attach De Bruijn indicies
              . unlist           -- expand multiple variables/arguments
            =<< cox              -- construct content expression from token tree
            =<< prefix htab      -- convert infix operator to prefix
            =<< syntax syn tree  -- expand syntax operator

debruijn :: B.Map (Cox c)
debruijn = de [] where
    de :: [String] -> B.Map (Cox c)
    de vars (B.Sourced src core) =
        B.Sourced src $ case core of
          CoxVar v _    -> maybe core (CoxVar v) $ indexFrom 1 v vars
          CoxApplyL _ _ -> mapToCox (de vars) core
          CoxDeriv v _  -> mapToCox (de $ v : vars) core
          _             -> core

indexFrom :: (Eq c) => Int -> c -> [c] -> Maybe Int
indexFrom origin key = loop origin where
    loop _ [] = Nothing
    loop i (x:xs) | x == key  = Just i
                  | otherwise = loop (i + 1) xs

unlist :: B.Map (Cox c)
unlist = derivL . (mapToCox unlist `fmap`) where
    derivL :: B.Map (Cox c)
    derivL e@(B.Sourced src2 core) =
        case core of
          CoxDerivL []     b -> b
          CoxDerivL (v:vs) b -> let sub = derivL $ B.Sourced src2 $ CoxDerivL vs b
                                in B.Sourced src2 $ CoxDeriv v sub
          _ -> e

-- construct content expression from token tree
cox :: forall c. (C.CContent c) => B.TokenTree -> B.Ab (Cox c)
cox = expr where
    expr tree = 
        B.abortableTree "cox" tree $
         let src = B.front $ B.untree tree
         in Right . B.Sourced src =<< core tree

    -- function application
    core :: B.TokenTree -> B.Ab (CoxCore c)
    core (B.TreeB 1 _ (fun : args)) =
        do fun'  <- cox fun
           args' <- cox `mapM` args
           Right $ CoxApplyL fun' args'

    -- function abstruction
    core (B.TreeB 6 _ [vars, b1]) =
        do b2 <- cox b1
           let vs = map B.tokenContent $ B.untree vars
           Right $ CoxDerivL vs b2
    core (B.TreeB 6 _ trees) =
        B.bug $ "core/abstruction: " ++ show (length trees)

    -- literal or variable
    core tree@(B.TreeL tok) =
        case tok of
          B.TTerm _ ns   ->  Right $ CoxTerm ns []
          B.TWord _ _ v  ->  case C.litContent tree of
                               Right c -> Right $ CoxLit c
                               Left _  -> Right $ CoxVar v 0
          _              ->  B.bug "core/leaf"

    -- literal composite
    core tree@(B.TreeB n _ _) | n > 1 = fmap CoxLit $ C.litContent tree
    core (B.TreeB n _ _) = Left $ B.AbortSyntax [] $ B.ASUnkCox $ show n

-- convert from infix to prefix
prefix :: [B.Named B.InfixHeight] -> B.AbMap (B.TokenTree)
prefix htab tree =
    B.abortableTree "prefix" tree $
     case B.infixToPrefix ht tree of
       Right tree3 -> Right $ B.undouble (== 1) tree3
       Left  xs    -> Left  $ B.AbortSyntax [] $ B.ASAmbInfixes $ map detail xs
    where
      ht = B.infixHeight wordText htab

      wordText :: B.Token -> Maybe String
      wordText (B.TWord _ 0 w) = Just w
      wordText _ = Nothing

      detail (Right n, tok) = detailText tok "right" n
      detail (Left  n, tok) = detailText tok "left"  n

      detailText tok dir n = B.tokenContent tok ++ " : " ++ dir ++ " " ++ show n

-- expand syntax operator
syntax :: forall c. [Cop c] -> B.AbMap B.TokenTree
syntax syn = expand where
    assoc :: [B.Named (Cop c)]
    assoc = map B.named syn

    expand tree@(B.TreeB 1 p subtrees) =
        case subtrees of
          op@(B.TreeL (B.TWord _ 0 name)) : args
              -> B.abortableTree "syntax" tree $
                 case lookup name assoc of
                   Just (CopSyn _ f) -> expand =<< f args
                   _                 -> do args2 <- mapM expand args
                                           Right $ B.TreeB 1 p (op : args2)
          _ -> do sub2 <- mapM expand subtrees
                  Right $ B.TreeB 1 p sub2

    expand (B.TreeB 6 p trees) =
        case B.divideTreesBy "|" trees of
          [vars, b1] -> do b2 <- expand $ B.treeWrap b1
                           Right $ B.TreeB 6 p [B.treeWrap vars, b2]
          _ -> Left $ B.AbortSyntax [] $ B.ASUnkCox "abstruction"
    expand tree = Right tree



-- ----------------------  beta construction

coxBeta :: forall c. [Cop c] -> [NamedCox c] -> B.Relhead -> Cox c -> B.Ab (Cox c)
coxBeta base deriv h cox1 =
    Right . subst             -- beta reduction
          . link base deriv   -- substitute free variables
        =<< position h cox1

-- beta reduction
subst :: B.Map (Cox c)
subst = su [] where
    su :: [Maybe (Cox c)] -> B.Map (Cox c)
    su args e@(B.Sourced src core) =
        let s = B.Sourced src
        in case core of
             CoxVar _ i    -> B.maybeWith e $ args !! (i - 1)
             CoxDeriv v b  -> s $ CoxDeriv v $ su (Nothing : args) b
             CoxApplyL _ _ -> app args $ s $ mapToCox (su args) core
             _             -> e

    app :: [Maybe (Cox c)] -> B.Map (Cox c)
    app args (B.Sourced _ (CoxApplyL (B.Sourced _ (CoxDeriv _ b)) (x:xs))) =
        app (Just x : args) $ B.Sourced [] (CoxApplyL b xs)
    app args (B.Sourced _ (CoxApplyL f [])) = su args f
    app _ e2 = e2

link :: forall c. [Cop c] -> [NamedCox c] -> B.Map (Cox c)
link base deriv = li where
    li e@(B.Sourced src core) =
        case core of
          CoxVar n 0 -> B.maybeWith e $ lookup n fs
          _          -> B.Sourced src $ mapToCox li core

    fs :: [NamedCox c]
    fs = map (fmap li) deriv ++ map namedBase base

    namedBase :: Cop c -> NamedCox c
    namedBase (CopFun n f) = (n, B.Sourced [] $ CoxBase n f)
    namedBase (CopSyn n _) = (n, B.Sourced [] $ CoxBase n undefined)

-- put term positions for actural heading
position :: B.Relhead -> Cox c -> B.Ab (Cox c)
position h = spos where
    spos = B.sourcedAbMap pos
    pos (CoxTerm ns _)  =
        let index = B.headIndex1 h ns
        in if all (>= 0) index
           then Right $ CoxTerm ns index
           else Left  $ B.AbortAnalysis [] (B.AANoTerms ns)
    pos (CoxApplyL f xs) = do f'  <- spos f
                              xs' <- mapM spos xs
                              Right $ CoxApplyL f' xs'
    pos e = Right e



-- ----------------------  Run

-- | Calculate content expression.
coxRun
  :: forall c. (C.CRel c, C.CList c, B.Pretty c)
  => [c]           -- ^ Tuple in body of relation
  -> Cox c         -- ^ Content expression
  -> B.Ab c        -- ^ Calculated literal content
coxRun args = run 0 where
    run :: Int -> Cox c -> B.Ab c
    run 1000 _ = B.bug "Too deep expression"
    run lv (B.Sourced src core) =
        let run' = run $ lv + 1
        in B.abortable "calc" src $
           case core of
             CoxLit c       -> Right c
             CoxTerm _ [p]  -> Right $ args !! p
             CoxTerm _ ps   -> term ps args
             CoxApplyL e [] -> run' e
             CoxApplyL (B.Sourced _ (CoxBase _ f)) xs -> f $ map run' xs
             _ -> Left $ B.abortNotFound $ "cox: " ++ show core

    term :: [Int] -> [c] -> B.Ab c
    term []       _ = Left $ B.abortNotFound "term"
    term (-1 : _) _ = Left $ B.abortNotFound "term"
    term (p : ps) args2 =
        let c = args2 !! p
        in if C.isRel c
           then rel ps $ C.gRel c
           else Right c

    rel :: [Int] -> B.Rel c -> B.Ab c
    rel ps (B.Rel _ args2) =
        C.putList =<< mapM (term ps) args2

checkIrreducible :: Cox c -> B.Ab (Cox c)
checkIrreducible e@(B.Sourced src _)
    | irreducible e = Right e
    | otherwise     = B.abortable "irrep" src $
                      Left $ B.AbortSyntax [] $ B.ASUnkCox "Not irreducible"

-- irreducible representation
irreducible :: Cox c -> Bool
irreducible (B.Sourced _ core) =
    case core of
      CoxLit  _       ->  True
      CoxTerm _ _     ->  True
      CoxBase _ _     ->  True
      CoxApplyL f xs  ->  all irreducible $ f : xs
      _               ->  False


-- ----------------------
-- $Process
--
--  Phase 1. Convert input texts into token trees.
--
--    [1. @String -> \[Token\]@]
--       Parse input string into list of token.
--       See 'B.tokens'.
--
--    [2. @\[Token\] -> \[TokenTree\]@]
--       Analyze token list structure.
--       See 'B.tokenTrees'
--
--    [3. @\[TokenTree\] -> TokenTree@]
--       Enclose list of token tree in 'B.TreeB'.
--       See 'B.treeWrap'.
--
--  Phase 2. Transform token trees,
--           and convert into abstract syntax trees.
--
--    [4. @TokenTree -> TokenTree@]
--       Expand syntax operators.
--
--    [5. @TokenTree -> TokenTree@]
--       Translate binary operators from infix to prefix.
--       See 'B.infixToPrefix'.
--
--    [6. @TokenTree -> Cox c@]
--       Convert from token tree into abstract syntax trees.
--
--  Phase 3. Calculate content expressions.
--
--    [7. @Cox c -> Cox c@]
--       Attach De Bruijn indecies for bound variables.
--
--    [8. @Relhead -> Cox c -> Cox c@]
--       Attach term positions using actural heading of relation.
--
--    [9. @Cox c -> Cox c@]
--       Reduce derived expressions into base expressions.
--
--    [10. @\[c\] -> Cox c -> c@]
--       Calculate content expression for each tuple of relation.
--       See 'coxRun'.


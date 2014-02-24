{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Term-content calcutation. -}

module Koshucode.Baala.Core.Content.Cox
( -- $Process

  -- * Expression
  Cox, CoxCore (..),
  -- * Operator
  Cop (..), CopFun, CopSyn,
  -- * Construction
  NamedCox,
  coxCons, checkIrreducible,
  -- * Run
  CoxCons, coxPosition, coxRun,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content.Literal as C


-- ----------------------  Expressions and operators

{-| Content expressions with source code. -}
type Cox c = B.Sourced (CoxCore c)

type NamedCox c = B.Named (Cox c)

{-| Content expressions. -}
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
    doc e = case e of
              CoxLit c       -> B.doc c
              CoxTerm ns _   -> B.doch ns
              CoxBase n _    -> B.doc n
              CoxVar v i     -> B.doc v B.<> B.doc "/" B.<> B.doc i
              CoxApplyL (B.Sourced _ f) xs ->
                  B.doc "ap" B.<+> B.doc f B.<+> B.doch (map B.unsourced xs)
              CoxDeriv  v  (B.Sourced _ b) -> fn (B.doc v)   (B.doc b)
              CoxDerivL vs (B.Sourced _ b) -> fn (B.doch vs) (B.doc b)
        where
          fn v b = B.docWraps "(|" "|)" $ v B.<+> B.doc "|" B.<+> b

mapToCox :: B.Map (Cox c) -> B.Map (CoxCore c)
mapToCox g (CoxApplyL f xs) = CoxApplyL (g f) (map g xs)
mapToCox g (CoxDeriv v b)   = CoxDeriv v (g b)
mapToCox g (CoxDerivL vs b) = CoxDerivL vs (g b)
mapToCox _ e = e

{-| Term-content operator. -}
data Cop c
    = CopFun String (CopFun c)  -- ^ Function
    | CopSyn String (CopSyn)    -- ^ Syntax

type CopFun c = [c] -> B.Ab c
type CopSyn   = CopFun B.TokenTree

instance Show (Cop c) where
    show (CopFun n _) = "(CopFun " ++ show n ++ " _)"
    show (CopSyn n _) = "(CopSyn " ++ show n ++ " _)"

instance B.Name (Cop c) where
    name (CopFun n _) = n
    name (CopSyn n _) = n



-- ----------------------  Construction

{-| Construct content expressions from token tree. -}
coxCons :: forall c. (C.CContent c)
  => ([Cop c], [B.Named B.InfixHeight]) -> [NamedCox c] -> B.TokenTree -> B.Ab (Cox c)
coxCons (cops, htab) deriv = body where
    body :: B.TokenTree -> B.Ab (Cox c)
    body tree =
        Right . subst            -- beta reduction
              . link cops deriv  -- substitute free variables
              . debruijn         -- attach De Bruijn indicies
              . unlist           -- expand multiple variables/arguments
            =<< expression       -- construct content expression from token tree
            =<< prefix htab      -- convert infix operator to prefix
            =<< syntax tree      -- expand syntax operator

    assoc :: [B.Named (Cop c)]
    assoc = map B.named cops

    -- expand syntax operator
    syntax :: B.AbMap B.TokenTree
    syntax tree@(B.TreeB 1 p subtrees) =
        case subtrees of
          op@(B.TreeL (B.TWord _ 0 name)) : args
            -> B.abortableTree "syntax" tree $
               case lookup name assoc of
                 Just (CopSyn _ f) -> syntax =<< f args
                 _                 -> do args2 <- mapM syntax args
                                         Right $ B.TreeB 1 p (op : args2)
          _ -> do sub2 <- mapM syntax subtrees
                  Right $ B.TreeB 1 p sub2
    syntax (B.TreeB 6 p trees) =
        case B.divideTreesBy "|" trees of
          [vars, b1] -> do b2 <- syntax $ B.treeWrap b1
                           Right $ B.TreeB 6 p [B.treeWrap vars, b2]
          _ -> Left $ B.AbortSyntax [] $ B.ASUnkCox "abstruction"
    syntax tree = Right tree

    -- construct content expression from token tree
    expression :: B.TokenTree -> B.Ab (Cox c)
    expression tree =
        B.abortableTree "cox" tree $
         let src = B.front $ B.untree tree
         in Right . B.Sourced src =<< core tree

    core :: B.TokenTree -> B.Ab (CoxCore c)
    core tree@(B.TreeL tok) =
        case tok of
          B.TTerm _ ns   ->  Right $ CoxTerm ns []
          B.TWord _ _ v  ->  case C.litContent tree of
                               Right c -> Right $ CoxLit c
                               Left _  -> Right $ CoxVar v 0
          _              ->  B.bug "core/leaf"

    -- function application
    core (B.TreeB 1 _ (fun : args)) =
        do fun'  <- expression fun
           args' <- expression `mapM` args
           Right $ CoxApplyL fun' args'

    -- function abstruction
    core (B.TreeB 6 _ [vars, b1]) =
        do b2 <- expression b1
           let vs = map B.tokenContent $ B.untree vars
           Right $ CoxDerivL vs b2
    core (B.TreeB 6 _ trees) =
        B.bug $ "core/abstruction: " ++ show (length trees)

    -- literal composite
    core tree@(B.TreeB n _ _) | n > 1 = fmap CoxLit $ C.litContent tree  
    core (B.TreeB n _ _) = Left $ B.AbortSyntax [] $ B.ASUnkCox $ show n
                
-- beta reduction
subst :: B.Map (Cox c)
subst = sub [] where
    sub :: [(Int, Cox c)] -> B.Map (Cox c)
    sub arg e@(B.Sourced src core) =
        let sub' = sub arg
            s    = B.Sourced src
        in case core of
             CoxVar _ i         -> maybe e id $ lookup i arg
             CoxDeriv v b       -> s $ CoxDeriv v $ sub (map inc arg) b
             CoxApplyL f []     -> sub' f
             CoxApplyL f (x:xs) -> let f'   = sub' f
                                       x'   = sub' x
                                       arg' = (1, x') : arg
                                   in case f' of
                                        B.Sourced _ (CoxDeriv _ b) ->
                                            sub arg' $ s $ CoxApplyL b xs
                                        _ -> s $ CoxApplyL f' (x' : map sub' xs)
             _                -> e

    inc :: (Int, Cox c) -> (Int, Cox c)
    inc (n, x) = (n + 1, x)

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


-- ----------------------  Link

link :: forall c. [Cop c] -> [NamedCox c] -> B.Map (Cox c)
link base deriv = li where
    li :: B.Map (Cox c)
    li e@(B.Sourced src core) =
        let s = B.Sourced src
        in case core of
             CoxVar n 0    -> maybe e id $ lookup n fs
             CoxDeriv _ _  -> s $ mapToCox li core
             CoxApplyL _ _ -> s $ mapToCox li core
             _             -> e

    fs :: [NamedCox c]
    fs = map (fmap li) deriv ++ map namedBase base

    namedBase :: Cop c -> NamedCox c
    namedBase (CopFun n f) = (n, B.Sourced [] $ CoxBase n f)
    namedBase (CopSyn n _) = (n, B.Sourced [] $ CoxBase n undefined)



-- ----------------------  Run

type CoxCons c = B.Relhead -> B.Ab (Cox c)

-- put term positions for actural heading
coxPosition :: Cox c -> CoxCons c
coxPosition scox h = spos scox where
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

{-| Calculate content expression. -}
coxRun
  :: forall c. (C.CRel c, C.CList c, B.Pretty c)
  => B.Relhead     -- ^ Heading of relation
  -> [c]           -- ^ Tuple in body of relation
  -> CoxCons c     -- ^ Content expression
  -> B.Ab c        -- ^ Calculated literal content
coxRun h arg pcox = run =<< checkIrreducible =<< pcox h where
    run :: Cox c -> B.Ab c
    run (B.Sourced src cox) = B.abortable "calc" src $
        case cox of
          CoxLit c      -> Right c
          CoxTerm _ [p] -> Right $ arg !! p
          CoxTerm _ ps  -> term ps arg
          CoxApplyL e [] -> run e
          CoxApplyL (B.Sourced _ (CoxBase _ f)) xs -> f =<< mapM run xs
          _             -> Left $ B.abortNotFound $ "cox: " ++ show cox

    term []       _ = Left $ B.abortNotFound "term"
    term (-1 : _) _ = Left $ B.abortNotFound "term"
    term (p : ps) arg2 =
        let c = arg2 !! p
        in if C.isRel c
           then rel ps $ C.getRel c
           else Right c

    rel ps (B.Rel _ args) = Right . C.putList =<< mapM (term ps) args


-- ----------------------
{- $Process

   Phase 1. From string to prefixed token tree.

     [1. @String -> \[Token\]@]
        Parse input string into list of token.
        See 'B.tokens'.

     [2. @\[Token\] -> \[Tree Token\]@]
        Analyze token list structure.
        See 'B.tokenTrees'

     [3. @\[Tree Token\] -> Tree Token@]
        Enclose list of token tree in 'B.TreeB'.
        See 'B.treeWrap'.

     [4. @Tree Token -> Tree Token@]
        Translate binary operators from infix to prefix.
        See 'B.infixToPrefix'.

   Phase 2. From prefixed token tree to literal content.

     [5. @Tree Token -> Cox c@]
        Convert from token tree to content expression.
        See 'coxSpecial'.

     [6. @Cox c -> (Relhead -> Cox c)@]
        Attach term positions using actural heading of relation.
        See 'coxSpecial'.

     [7. @Cox c -> \[c\] -> c@]
        Calculate content expression for each tuple of relation.
        See 'coxRun'.

-}


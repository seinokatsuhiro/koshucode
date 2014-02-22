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
  CoxAssoc,
  coxCons, checkIrreducible,
  -- * Run
  CoxCons, coxPosition, coxRun,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content.Literal as C


-- ----------------------  Expressions and operators

{-| Content expressions with source code. -}
type Cox c = B.Sourced (CoxCore c)

{-| Content expressions. -}
data CoxCore c
    = CoxLit c                      -- ^ A: Literal content
    | CoxTerm [B.Termname] [Int]    -- ^ A: Term reference, its name and position
    | CoxCall (CopFun c) [Cox c]    -- ^ A: Call content operator

    | CoxVariable Int String        -- ^ B: Variable, its De Bruijn index and name
    | CoxApply   (Cox c)  (Cox c)   -- ^ B: Function application
    | CoxLambda   String  (Cox c)   -- ^ B: Function abstraction

    | CoxApplyL  (Cox c)  [Cox c]   -- ^ C: Function application (multiple arguments)
    | CoxLambdaL [String] (Cox c)   -- ^ C: Function abstraction (multiple variables)

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

type CoxAssoc c = [B.Named (Cox c)]

{-| Construct content expressions from token tree. -}
coxCons :: forall c. (C.CContent c)
  => ([Cop c], [B.Named B.InfixHeight]) -> CoxAssoc c -> B.TokenTree -> B.Ab (Cox c)
coxCons (cops, htab) dict = body where
    body :: B.TokenTree -> B.Ab (Cox c)
    body tree =
        Right . beta          -- beta reduction
              . link dict     -- substitute free variables
              . debruijn      -- attach De Bruijn indicies
              . unlist        -- expand multiple variables/arguments
            =<< expression    -- construct content expression from token tree
            =<< prefix htab   -- convert infix operator to prefix
            =<< syntax tree   -- expand syntax operator

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
                               Left _  -> Right $ CoxVariable 0 v
          _              ->  B.bug "core/leaf"

    -- parend unquoted word and its arguments
    core (B.TreeB 1 _ (B.TreeL (B.TWord _ 0 name) : args)) =
        case lookup name assoc of
          Just (CopFun _ f) -> CoxCall f `fmap` (expression `mapM` args)
          Just (CopSyn _ _) -> B.bug "core/copsyn"
          Nothing           -> do args' <- expression `mapM` args
                                  Right $ CoxApplyL (B.Sourced [] $ CoxVariable 0 name) args'
          --Nothing -> Left $ B.AbortAnalysis [] $ B.AAUnkCop name

    -- function application
    core (B.TreeB 1 _ (fun : args)) =
        do fun'  <- expression fun
           args' <- expression `mapM` args
           Right $ CoxApplyL fun' args'

    -- function abstruction
    core (B.TreeB 6 _ [vars, b1]) =
        do b2 <- expression b1
           let vs = map B.tokenContent $ B.untree vars
           Right $ CoxLambdaL vs b2
    core (B.TreeB 6 _ trees) =
        B.bug $ "core/abstruction: " ++ show (length trees)

    -- literal composite
    core tree@(B.TreeB n _ _) | n > 1 = fmap CoxLit $ C.litContent tree  
    core (B.TreeB n _ _) = Left $ B.AbortSyntax [] $ B.ASUnkCox $ show n

beta :: B.Map (Cox c)
beta = be [] where
    be :: [(Int, Cox c)] -> B.Map (Cox c)
    be binds e@(B.Sourced src core) =
        let be' = be binds
            source = B.Sourced src
        in case core of
             CoxCall n xs     -> source $ CoxCall n $ map be' xs
             CoxVariable i _  -> maybe e id $ lookup i binds
             CoxLambda v b    -> source $ CoxLambda v $ be (map inc binds) b
             CoxApply f x     -> let x' = be' x
                                 in case be' f of
                                      B.Sourced _ (CoxLambda _ b) ->
                                          be ((1, x') : binds) b
                                      f' -> source $ CoxApply f' x'
             _                -> e

    inc :: (Int, Cox c) -> (Int, Cox c)
    inc (n, x) = (n + 1, x)

debruijn :: B.Map (Cox c)
debruijn = de [] where
    de :: [String] -> B.Map (Cox c)
    de vars (B.Sourced src core) =
        let de' = de vars
        in B.Sourced src $ case core of
             CoxCall n xs     -> CoxCall n $ map de' xs
             CoxVariable _ v  -> case indexFrom 1 v vars of
                                   Just i  -> CoxVariable i v
                                   Nothing -> core
             CoxApply f x     -> CoxApply (de' f) (de' x)
             CoxLambda v b    -> CoxLambda v $ de (v : vars) b
             _                -> core

unlist :: B.Map (Cox c)
unlist e@(B.Sourced src core) =
    case core of
      CoxApplyL  f xs -> applyL  $ B.Sourced src (CoxApplyL (unlist f) (map unlist xs))
      CoxLambdaL vs b -> lambdaL $ B.Sourced src (CoxLambdaL vs (unlist b))
      _              -> e
    where
      applyL :: B.Map (Cox c)
      applyL e2@(B.Sourced src2 core2) =
          case core2 of
            CoxApplyL f [] -> f
            CoxApplyL f (x:xs) ->
                applyL $ B.Sourced src2 $ CoxApplyL (B.Sourced src $ CoxApply f x) xs
            _ -> e2

      lambdaL :: B.Map (Cox c)
      lambdaL e2@(B.Sourced src2 core2) =
          case core2 of
            CoxLambdaL [] b -> b
            CoxLambdaL (v:vs) b ->
                B.Sourced src2 $ CoxLambda v $ lambdaL $ B.Sourced src $ CoxLambdaL vs b
            _ -> e2

indexFrom :: (Eq c) => Int -> c -> [c] -> Maybe Int
indexFrom origin key = loop origin where
    loop _ [] = Nothing
    loop i (x:xs) | x == key  = Just i
                  | otherwise = loop (i + 1) xs

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

link :: forall c. CoxAssoc c -> B.Map (Cox c)
link dict = li where
    li :: B.Map (Cox c)
    li e@(B.Sourced src core) =
        let s = B.Sourced src
        in case core of
             CoxCall n xs    -> s $ CoxCall n $ map li xs
             CoxVariable 0 n -> maybe e id $ lookup n dictrec
             CoxLambda v b   -> s $ CoxLambda v (li b)
             CoxApply f x    -> s $ CoxApply (li f) (li x)
             _               -> e

    dictrec :: CoxAssoc c
    dictrec = mapSnd li dict

mapSnd :: (b -> b) -> [(a, b)] -> [(a, b)]
mapSnd = map . fmap



-- ----------------------  Run

type CoxCons c = B.Relhead -> B.Ab (Cox c)

-- put term positions for actural heading
coxPosition :: Cox c -> CoxCons c
coxPosition scox h = spos scox where
    spos = B.sourcedAbMap pos
    pos (CoxCall f cs)   = Right . CoxCall f =<< mapM spos cs
    pos (CoxTerm ns _)  =
        let index = B.headIndex1 h ns
        in if all (>= 0) index
           then Right $ CoxTerm ns index
           else Left  $ B.AbortAnalysis [] (B.AANoTerms ns)
    pos e = Right e

checkIrreducible :: Cox c -> B.Ab (Cox c)
checkIrreducible e
    | irreducible e = Right e
    | otherwise     = Left $ B.AbortSyntax [] $ B.ASUnkCox "Not irreducible"

-- irreducible representation
irreducible :: Cox c -> Bool
irreducible (B.Sourced _ core) =
    case core of
      CoxLit  _     ->  True
      CoxTerm _ _   ->  True
      CoxCall _ xs  ->  all irreducible xs
      _             ->  False

{-| Calculate content expression. -}
coxRun
  :: forall c. (C.CRel c, C.CList c)
  => B.Relhead     -- ^ Heading of relation
  -> [c]           -- ^ Tuple in body of relation
  -> CoxCons c     -- ^ Content expression
  -> B.Ab c        -- ^ Calculated literal content
coxRun h arg pcox = run =<< pcox h where
    run :: Cox c -> B.Ab c
    run (B.Sourced src cox) = B.abortable "calc" src $
        case cox of
          CoxLit c      -> Right c
          CoxTerm _ [p] -> Right $ arg !! p
          CoxTerm _ ps  -> term ps arg
          CoxCall f cs  -> f =<< mapM run cs
          _             -> Left $ B.abortNotFound ""

    term []       _ = Left $ B.abortNotFound ""
    term (-1 : _) _ = Left $ B.abortNotFound ""
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


{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content calcutation.

module Koshucode.Baala.Core.Content.Cox
( -- $Process

  -- * Expression
  Cox (..),
  coxSyntacticArity,
  isCoxBase,
  isCoxDeriv,

  -- * Operator
  Cop (..), CopFun, CopCox, CopSyn,
  isCopFunction,
  isCopSyntax,

  -- * Construction
  NamedCox,
  coxAlpha, coxDebruijn, coxBeta,
  checkIrreducible,

  -- * Run
  getArg1, getArg2, getArg3,
  coxRun,
) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Content.Class     as C
import qualified Koshucode.Baala.Core.Content.Literal   as C
import qualified Koshucode.Baala.Core.Message           as Message


-- ----------------------  Expression

type NamedCox c = B.Named (Cox c)

-- | Content expressions.
--   R: reduced form, R\/B: pre-beta form, R\/B\/A: pre-alpha form.
data Cox c
    = CoxLit    [B.CodePoint] c                   -- ^ R:   Literal content
    | CoxTerm   [B.CodePoint] [B.TermName] [Int]  -- ^ R:   Term reference, its name and position
    | CoxBase   [B.CodePoint] String (CopFun c)   -- ^ R:   Base function
    | CoxVar    [B.CodePoint] String Int          -- ^ B:   Local (> 0) or global (= 0) variable,
                                                  --        its name and De Bruijn index
    | CoxApplyL [B.CodePoint] (Cox c) [Cox c]     -- ^ R/B: Function application (multiple arguments)
    | CoxDeriv  [B.CodePoint] String  (Cox c)     -- ^ B:   Derived function (single variable)
    | CoxDerivL [B.CodePoint] [String] (Cox c)    -- ^ A:   Derived function (multiple variables)

instance B.CodePointer (Cox c) where
    codePoint (CoxLit    pt _)   = pt
    codePoint (CoxTerm   pt _ _) = pt
    codePoint (CoxBase   pt _ _) = pt
    codePoint (CoxVar    pt _ _) = pt
    codePoint (CoxApplyL pt _ _) = pt
    codePoint (CoxDeriv  pt _ _) = pt
    codePoint (CoxDerivL pt _ _) = pt

instance (B.Write c) => Show (Cox c) where
    show = show . B.doc

instance (B.Write c) => B.Write (Cox c) where
    write = docCox

docCox :: (B.Write c) => B.StringMap -> Cox c -> B.Doc
docCox sh = d (0 :: Int) where
    d 10 _ = B.write sh "..."
    d n e =
        case e of
          CoxLit    _ c      -> B.write sh c
          CoxTerm   _ ns _   -> B.writeH sh ns
          CoxBase   _ name _ -> B.write sh name
          CoxVar    _ v i    -> B.write sh v B.<> B.write sh "/" B.<> B.write sh i
          CoxApplyL _ f xs   -> let f'  = B.write sh "ap" B.<+> d' f
                                    xs' = B.nest 3 $ B.writeV sh (map d' xs)
                                in f' B.$$ xs'
          CoxDeriv  _ v  e2  -> fn (B.write  sh v)  (d' e2)
          CoxDerivL _ vs e2  -> fn (B.writeH sh vs) (d' e2)
        where
          d'     = d $ n + 1
          fn v b = B.docWraps "(|" "|)" $ v B.<+> B.write sh "|" B.<+> b

mapToCox :: B.Map (Cox c) -> B.Map (Cox c)
mapToCox g (CoxApplyL src f  xs)   = CoxApplyL src (g f) (map g xs)
mapToCox g (CoxDeriv  src v  body) = CoxDeriv  src v  (g body)
mapToCox g (CoxDerivL src vs body) = CoxDerivL src vs (g body)
mapToCox _ e = e

isCoxBase :: Cox c -> Bool
isCoxBase (CoxBase _  _ _)   = True
isCoxBase _                  = False

isCoxDeriv :: Cox c -> Bool
isCoxDeriv (CoxDeriv  _ _ _) = True
isCoxDeriv (CoxDerivL _ _ _) = True
isCoxDeriv _                 = False

coxSyntacticArity :: Cox c -> Int
coxSyntacticArity = loop where
    loop (CoxDerivL _ vs cox)  = loop cox + length vs
    loop (CoxDeriv  _ _  cox)  = loop cox + 1
    loop (CoxApplyL _ cox xs)
        | isCoxDeriv cox = loop cox - length xs
        | otherwise      = 0
    loop _ = 0


-- ----------------------  Operator

-- | Content operator.
data Cop c
    = CopFun String (CopFun c)   -- ^ Convert contents
    | CopCox String (CopCox c)   -- ^ Convert coxes
    | CopSyn String (CopSyn)     -- ^ Convert trees

type CopFun c = [B.Ab c] -> B.Ab c
type CopCox c = [Cox c] -> B.Ab (Cox c)
type CopSyn   = [B.TokenTree] -> B.Ab B.TokenTree

instance Show (Cop c) where
    show (CopFun n _) = "(CopFun " ++ show n ++ " _)"
    show (CopCox n _) = "(CopCox " ++ show n ++ " _)"
    show (CopSyn n _) = "(CopSyn " ++ show n ++ " _)"

instance B.Name (Cop c) where
    name (CopFun n _) = n
    name (CopCox n _) = n
    name (CopSyn n _) = n

isCopFunction :: Cop c -> Bool
isCopFunction (CopFun _ _) = True
isCopFunction _            = False

isCopSyntax :: Cop c -> Bool
isCopSyntax (CopSyn _ _)   = True
isCopSyntax (CopCox _ _)   = True
isCopSyntax _              = False


-- ----------------------  Alpha construction

-- | Construct content expression from token tree
coxAlpha :: forall c. (C.CContent c)
  => ([Cop c], [B.Named B.InfixHeight]) -> B.TokenTree -> B.Ab (Cox c)
coxAlpha (syn, htab) =
    convCox syn           -- convert cox to cox
      B.<=< Right
      . debruijn          -- attach De Bruijn indicies
      . unlist            -- expand multiple variables/arguments
      B.<=< construct     -- construct content expression from token tree
      B.<=< prefix htab   -- convert infix operator to prefix
      B.<=< convTree syn  -- convert token tree to token tree

debruijn :: B.Map (Cox c)
debruijn = de [] where
    de :: [String] -> B.Map (Cox c)
    de vars cox =
        case cox of
          CoxVar src v _  -> maybe cox (CoxVar src v) $ indexFrom 1 v vars
          CoxApplyL _ _ _ -> mapToCox (de vars) cox
          CoxDeriv _ v _  -> mapToCox (de $ v : vars) cox
          _               -> cox

indexFrom :: (Eq c) => Int -> c -> [c] -> Maybe Int
indexFrom origin key = loop origin where
    loop _ [] = Nothing
    loop i (x:xs) | x == key  = Just i
                  | otherwise = loop (i + 1) xs

unlist :: B.Map (Cox c)
unlist = derivL . (mapToCox unlist) where
    derivL :: B.Map (Cox c)
    derivL e =
        case e of
          CoxDerivL _ []       b -> b
          CoxDerivL src (v:vs) b -> let sub = derivL $ CoxDerivL src vs b
                                    in CoxDeriv src v sub
          _ -> e

convCox :: forall c. [Cop c] -> B.AbMap (Cox c)
convCox syn = expand where
    assn :: [B.Named (Cop c)]
    assn = map B.named syn

    expand :: B.AbMap (Cox c)
    expand cox =
        case cox of
            CoxDeriv  src n  x -> Right . CoxDeriv  src n  =<< expand x
            CoxDerivL src ns x -> Right . CoxDerivL src ns =<< expand x
            CoxApplyL src f@(CoxVar _ n 0) xs ->
                case lookup n assn of
                  Just (CopCox _ g) -> expand =<< g xs
                  _                 -> expandApply src f xs
            CoxApplyL src f xs      -> expandApply src f xs
            _                       -> Right cox

    expandApply src f xs =
        do f'  <- expand f
           xs' <- mapM expand xs
           Right $ CoxApplyL src f' xs'
    
-- construct content expression from token tree
construct :: forall c. (C.CContent c) => B.TokenTree -> B.Ab (Cox c)
construct = expr where
    expr tree = 
        B.abortableTree "cox" tree $
         let src = concatMap B.codePoint $ B.front $ B.untree tree
         in cons src tree

    -- function application
    cons :: [B.CodePoint] -> B.TokenTree -> B.Ab (Cox c)
    cons src (B.TreeB 1 _ (fun : args)) =
        do fun'  <- construct fun
           args' <- construct `mapM` args
           Right $ CoxApplyL src fun' args'

    -- function abstruction
    cons src (B.TreeB 6 _ [vars, b1]) =
        do b2 <- construct b1
           let vs = map B.tokenContent $ B.untree vars
           Right $ CoxDerivL src vs b2
    cons _ (B.TreeB 6 _ trees) =
        B.bug $ "core/abstruction: " ++ show (length trees)

    -- literal or variable
    cons src tree@(B.TreeL tok) =
        case tok of
          B.TTerm _ ns   ->  Right $ CoxTerm src ns []
          B.TText _ _ v  ->  case C.litContent tree of
                               Right c -> Right $ CoxLit src c
                               Left _  -> Right $ CoxVar src v 0
          _              ->  B.bug "core/leaf"

    -- literal composite
    cons src tree@(B.TreeB n _ _) | n > 1 = fmap (CoxLit src) $ C.litContent tree
    cons _ (B.TreeB n _ _) = Message.unkCox $ show n

-- convert from infix to prefix
prefix :: [B.Named B.InfixHeight] -> B.AbMap (B.TokenTree)
prefix htab tree =
    B.abortableTree "prefix" tree $
     case B.infixToPrefix ht tree of
       Right tree3 -> Right $ B.undouble (== 1) tree3
       Left  xs    -> Message.ambInfixes $ map detail xs
    where
      ht = B.infixHeight wordText htab

      wordText :: B.Token -> Maybe String
      wordText (B.TText _ 0 w) = Just w
      wordText _ = Nothing

      detail (Right n, tok) = detailText tok "right" n
      detail (Left  n, tok) = detailText tok "left"  n

      detailText tok dir n = B.tokenContent tok ++ " : " ++ dir ++ " " ++ show n

-- expand syntax operator
convTree :: forall c. [Cop c] -> B.AbMap B.TokenTree
convTree syn = expand where
    assoc :: [B.Named (Cop c)]
    assoc = map B.named syn

    expand tree@(B.TreeB 1 p subtrees) =
        case subtrees of
          op@(B.TreeL (B.TText _ 0 name)) : args
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
          _ -> Message.unkCox "abstruction"
    expand tree = Right tree

coxDebruijn :: B.Map (Cox c)
coxDebruijn = comer . deepen . unlist where

comer :: B.Map (Cox c)
comer = de [] where
    de :: [String] -> B.Map (Cox c)
    de vars cox =
        case cox of
          CoxVar src v i
              | i >  0  -> CoxVar src v i
              | i == 0  -> maybe cox (CoxVar src v) $ indexFrom 1 v vars
          CoxApplyL _ _ _ -> mapToCox (de vars) cox
          CoxDeriv _ v _  -> mapToCox (de $ v : vars) cox
          _                -> cox

deepen :: B.Map (Cox c)
deepen = level (0 :: Int) where
    level n (CoxDeriv src v e)
        = CoxDeriv src v $ level (n + 1) e
    level n cox = de n [] cox

    de :: Int -> [String] -> B.Map (Cox c)
    de n vars cox =
        case cox of
          CoxVar src v i
              | i == 0  -> CoxVar src v 0
              | i >  0  -> if v `elem` vars
                           then CoxVar src v i        -- bound variable
                           else CoxVar src v (i + n)  -- free variable
          CoxApplyL _ _ _ -> mapToCox (de n vars) cox
          CoxDeriv _ v _  -> mapToCox (de n $ v : vars) cox
          _               -> cox


-- ----------------------  Beta construction

-- | Reduce content expression.
coxBeta :: [Cop c] -> [NamedCox c] -> B.Relhead -> B.AbMap (Cox c)
coxBeta base deriv h =
    Right . subst             -- beta reduction
          . link base deriv   -- substitute free variables
          B.<=< position h

-- beta reduction
subst :: B.Map (Cox c)
subst = su [] where
    su :: [Maybe (Cox c)] -> B.Map (Cox c)
    su args cox =
        case cox of
          CoxVar    _ _ i     -> B.fromMaybe cox $ args !!! (i - 1)
          CoxDeriv  src v e2  -> CoxDeriv src v $ su (Nothing : args) e2
          CoxApplyL _ _ _     -> app args $ mapToCox (su args) cox
          _                   -> cox

    app :: [Maybe (Cox c)] -> B.Map (Cox c)
    app args (CoxApplyL src (CoxDeriv _ _ b) (x:xs)) =
        app (Just x : args) (CoxApplyL src b xs)
    app args (CoxApplyL _ f []) = su args f
    app _ e2 = e2

link :: forall c. [Cop c] -> [NamedCox c] -> B.Map (Cox c)
link base deriv = li where
    li cox =
        case cox of
          CoxVar _ n 0 -> B.fromMaybe cox $ lookup n fs
          _            -> mapToCox li cox

    fs :: [NamedCox c]
    fs = map (fmap li) deriv ++ map namedBase base

    namedBase :: Cop c -> NamedCox c
    namedBase (CopFun n f) = (n, CoxBase [] n f)
    namedBase (CopCox n _) = (n, CoxBase [] n undefined)
    namedBase (CopSyn n _) = (n, CoxBase [] n undefined)

-- put term positions for actural heading
position :: B.Relhead -> Cox c -> B.Ab (Cox c)
position he = spos where
    spos e = B.abortable "position" [e] $ pos e
    pos (CoxTerm src ns _) =
        let index = B.headIndex1 he ns
        in if all (>= 0) index
           then Right $ CoxTerm src ns index
           else Message.unkTerm [B.showNestedTermName ns] he
    pos (CoxApplyL src  f xs) = do f'  <- spos f
                                   xs' <- mapM spos xs
                                   Right $ CoxApplyL src f' xs'
    pos (CoxDeriv src  v e)   = do e' <- spos e
                                   Right $ CoxDeriv src v e'
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

-- | Calculate content expression.
coxRun
  :: forall c. (C.CRel c, C.CList c, B.Write c)
  => [c]           -- ^ Tuple in body of relation
  -> Cox c         -- ^ Content expression
  -> B.Ab c        -- ^ Calculated literal content
coxRun args = run 0 where
    run :: Int -> Cox c -> B.Ab c
    run 1000 _ = B.bug "Too deep expression"
    run lv cox =
        let run' = run $ lv + 1
        in B.abortable "calc" [cox] $
           case cox of
             CoxLit    _ c       -> Right c
             CoxTerm   _ _ [p]   -> Right $ args !!! p
             CoxTerm   _ _ ps    -> term ps args
             CoxApplyL _ e []    -> run' e
             CoxApplyL _ (CoxBase _ _ f) xs -> f $ map run' xs
             _ -> Message.notFound $ "cox: " ++ show cox

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

checkIrreducible :: B.AbMap (Cox c)
checkIrreducible e
    | irreducible e = Right e
    | otherwise     = B.abortable "irrep" [e] $
                      Message.unkCox "Not irreducible"

-- irreducible representation
irreducible :: Cox c -> Bool
irreducible cox =
    case cox of
      CoxLit  _ _       ->  True
      CoxTerm _ _ _     ->  True
      CoxBase _ _ _     ->  True
      CoxApplyL _ f xs  ->  all irreducible $ f : xs
      _                 ->  False

(!!!) :: [a] -> Int -> a
list !!! index = loop index list where
    loop 0 (x : _)  = x
    loop i (_ : xs) = loop (i - 1) xs
    loop _ _        = error $ "#" ++ show (length list)
                                  ++ " !!! " ++ show index


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


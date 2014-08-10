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
  mapToCox,
  checkIrreducible,

  -- * Operator
  Cop (..), CopBundle, NamedCox,
  CopFun, CopCox, CopTree,
  isCopFunction,
  isCopSyntax,
) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Core.Message    as Message


-- ----------------------  Expression

-- | Content expressions.
--   R: reduced form, R\/B: pre-beta form, R\/B\/A: pre-alpha form.
data Cox c
    = CoxLit    [B.CodePoint] c                   -- ^ R:   Literal content
    | CoxTerm   [B.CodePoint] [B.TermName] [Int]  -- ^ R:   Term reference, its name and position
    | CoxBase   [B.CodePoint] String (CopFun c)   -- ^ R:   Base function
    | CoxVar    [B.CodePoint] String Int          -- ^ B:   Local (> 0) or global (= 0) variable,
                                                  --        its name and De Bruijn index
    | CoxApplyL [B.CodePoint] (Cox c)  [Cox c]    -- ^ R/B: Function application (multiple arguments)
    | CoxDeriv  [B.CodePoint]  String  (Cox c)    -- ^ B:   Derived function (single variable)
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
          CoxLit    _ c      -> B.write sh "lit" B.<+> B.write sh c
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

mapToCox :: B.Map (Cox c) -> B.Map (Cox c)
mapToCox g (CoxApplyL src f  xs)   = CoxApplyL src (g f) (map g xs)
mapToCox g (CoxDeriv  src v  body) = CoxDeriv  src v  (g body)
mapToCox g (CoxDerivL src vs body) = CoxDerivL src vs (g body)
mapToCox _ e = e

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


-- ----------------------  Operator

-- | Content operator.
data Cop c
    = CopFun  String (CopFun c)   -- ^ Convert contents
    | CopCox  String (CopCox c)   -- ^ Convert coxes
    | CopTree String (CopTree)    -- ^ Convert trees

-- | Base and derived operators.
type CopBundle c = ( [Cop c], [NamedCox c] )

type NamedCox c = B.Named (Cox c)

type CopFun c = [B.Ab c]      -> B.Ab c
type CopCox c = [Cox c]       -> B.Ab (Cox c)
type CopTree  = [B.TokenTree] -> B.Ab B.TokenTree

instance Show (Cop c) where
    show (CopFun  n _) = "(CopFun "  ++ show n ++ " _)"
    show (CopCox  n _) = "(CopCox "  ++ show n ++ " _)"
    show (CopTree n _) = "(CopTree " ++ show n ++ " _)"

instance B.Name (Cop c) where
    name (CopFun  n _) = n
    name (CopCox  n _) = n
    name (CopTree n _) = n

isCopFunction :: Cop c -> Bool
isCopFunction (CopFun _ _) = True
isCopFunction _            = False

isCopSyntax :: Cop c -> Bool
isCopSyntax (CopTree _ _)  = True
isCopSyntax (CopCox  _ _)  = True
isCopSyntax _              = False


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


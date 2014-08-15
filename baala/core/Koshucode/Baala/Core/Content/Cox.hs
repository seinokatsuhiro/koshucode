{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content calcutation.

module Koshucode.Baala.Core.Content.Cox
( -- $Process

  -- * Expression
  Cox (..), NamedCox,
  coxSyntacticArity,
  isCoxBase,
  isCoxForm,
  coxMap, coxCall,
  checkIrreducible,

  -- * Operator
  Cop (..), CopBundle,
  CopFun, CopCox, CopTree,
  copPrefix, copInfix, copPostfix,
  isCopFunction,
  isCopSyntax,
) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Core.Message    as Message


-- ----------------------  Expression

-- | Content expressions.
data Cox c
    = CoxLit    [B.CodePoint] c                   -- ^ Literal content
    | CoxTerm   [B.CodePoint] [B.TermName] [Int]  -- ^ Term reference, its name and position
    | CoxBase   [B.CodePoint] String (CopFun c)   -- ^ Base function
    | CoxLocal  [B.CodePoint] String Int          -- ^ Local blank, its name and De Bruijn index
    | CoxBlank  [B.CodePoint] String              -- ^ Blank in form
    | CoxRefill [B.CodePoint] (Cox c) [Cox c]     -- ^ Refill arguments in a form
    | CoxForm1  [B.CodePoint] (Maybe String)  String  (Cox c)  -- ^ Form with single blank
    | CoxForm   [B.CodePoint] (Maybe String) [String] (Cox c)  -- ^ Form with multiple blanks

type NamedCox c = B.Named (Cox c)

instance B.CodePointer (Cox c) where
    codePoint (CoxLit    cp _)      =  cp
    codePoint (CoxTerm   cp _ _)    =  cp
    codePoint (CoxBase   cp _ _)    =  cp
    codePoint (CoxLocal  cp _ _)    =  cp
    codePoint (CoxBlank  cp _)      =  cp
    codePoint (CoxRefill cp _ _)    =  cp
    codePoint (CoxForm1  cp _ _ _)  =  cp
    codePoint (CoxForm   cp _ _ _)  =  cp

instance (B.Write c) => Show (Cox c) where
    show = show . B.doc

instance (B.Write c) => B.Write (Cox c) where
    write = docCox

docCox :: (B.Write c) => B.StringMap -> Cox c -> B.Doc
docCox sh = d (0 :: Int) . derivL where
    d 10 _ = B.write sh "..."
    d n e =
        case e of
          CoxLit    _ c        -> B.write sh "lit" B.<+> B.write sh c
          CoxTerm   _ ns _     -> B.writeH sh ns
          CoxBase   _ name _   -> B.write sh name
          CoxLocal  _ v i      -> B.write sh v B.<> B.write sh "/" B.<> B.write sh i
          CoxBlank  _ v        -> B.write sh v
          CoxRefill _ f xs     -> let f'  = B.write sh "ap" B.<+> d' f
                                      xs' = B.nest 3 $ B.writeV sh (map d' xs)
                                  in f' B.$$ xs'
          CoxForm1  _ tag v  e2  -> fn tag (B.write  sh v)  (d' e2)
          CoxForm   _ tag vs e2  -> fn tag (B.writeH sh vs) (d' e2)
        where
          d' = d $ n + 1
          fn Nothing    v b  =  B.docWraps "(|" "|)" $ v B.<+> B.write sh "|" B.<+> b
          fn (Just tag) v b  =  B.docWraps "(|" "|)" $ (B.write sh $ "'" ++ tag)
                                                       B.<+> v B.<+> B.write sh "|" B.<+> b

-- Convert CoxForm1 to CoxForm.
derivL :: B.Map (Cox c)
derivL (CoxForm1 cp1 tag1 v1 e1) =
    case derivL e1 of
      CoxForm _ tag2 vs e2 | tag1 == tag2  ->  CoxForm cp1 tag1 (v1:vs) e2
      e2                                   ->  CoxForm cp1 tag1 [v1]    e2
derivL (CoxRefill cp f xs) = CoxRefill cp (derivL f) (map derivL xs)
derivL e = e

isCoxBase :: Cox c -> Bool
isCoxBase (CoxBase _  _ _)   = True
isCoxBase _                  = False

isCoxForm :: Cox c -> Bool
isCoxForm (CoxForm1  _ _ _ _) = True
isCoxForm (CoxForm   _ _ _ _) = True
isCoxForm _                   = False

coxSyntacticArity :: Cox c -> Int
coxSyntacticArity = loop where
    loop (CoxForm   _ _ vs cox)  = loop cox + length vs
    loop (CoxForm1  _ _ _  cox)  = loop cox + 1
    loop (CoxRefill _ cox xs)
        | isCoxForm cox  = loop cox - length xs
        | otherwise      = 0
    loop _ = 0

coxMap :: B.Map (B.Map (Cox c))
coxMap g (CoxRefill cp f xs)        = CoxRefill cp (g f) (map g xs)
coxMap g (CoxForm1  cp tag v  body) = CoxForm1  cp tag v  (g body)
coxMap g (CoxForm   cp tag vs body) = CoxForm   cp tag vs (g body)
coxMap _ e = e

coxCall :: Cox c -> (B.Map (Cox c)) -> Cox c
coxCall cox g = coxMap g cox

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
      CoxRefill _ f xs  ->  all irreducible $ f : xs
      _                 ->  False


-- ----------------------  Operator

-- | Content operator.
data Cop c
    = CopFun  String (CopFun c)   -- ^ Convert contents
    | CopCox  String (CopCox c)   -- ^ Convert coxes
    | CopTree String (CopTree)    -- ^ Convert trees

-- | Base and derived operators.
type CopBundle c = ( [Cop c], [NamedCox c] )

-- | Base function.
type CopFun c = [B.Ab c] -> B.Ab c

-- | Expression-level syntax.
type CopCox c = [Cox c] -> B.Ab (Cox c)

-- | Tree-level syntax.
type CopTree  = [B.TokenTree] -> B.Ab B.TokenTree

instance Show (Cop c) where
    show (CopFun  n _) = "(CopFun "  ++ show n ++ " _)"
    show (CopCox  n _) = "(CopCox "  ++ show n ++ " _)"
    show (CopTree n _) = "(CopTree " ++ show n ++ " _)"

instance B.Name (Cop c) where
    name (CopFun  n _) = n
    name (CopCox  n _) = n
    name (CopTree n _) = n

-- | Convert operator name to prefix name
copPrefix  :: B.Map String
copPrefix  = ("pre|" ++)

-- | Convert operator name to postfix name
copPostfix :: B.Map String
copPostfix = ("post|" ++)

-- | Convert operator name to infix name
copInfix   :: B.Map String
copInfix   = ("in|" ++)

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


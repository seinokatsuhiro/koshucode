{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content calcutation.

module Koshucode.Baala.Core.Content.Cox
( -- $Process

  -- * Expression
  Cox (..), CoxAssn, NamedCox,
  coxSyntacticArity,
  isCoxBase,
  isCoxForm,
  coxMap, coxCall,
  checkIrreducible,

  -- * Operator
  Cop (..), CopBundle,
  CopFun, CopCox, CopTree,
  copName,
  copNormal, copInternal, copPrefix, copInfix, copPostfix,
  isCopFunction, isCopSyntax,
) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Core.Message    as Message


-- ----------------------  Expression

-- | Content expressions.
data Cox c
    = CoxLit    [B.CodePoint] c                       -- ^ Literal content
    | CoxTerm   [B.CodePoint] [B.TermName] [Int]      -- ^ Term reference, its name and position
    | CoxBase   [B.CodePoint] (Cop c)                 -- ^ Base function
    | CoxLocal  [B.CodePoint] String Int              -- ^ Local blank, its name and De Bruijn index
    | CoxBlank  [B.CodePoint] B.BlankName             -- ^ Blank in form
    | CoxRefill [B.CodePoint] (Cox c) [Cox c]         -- ^ Refill arguments in a form
    | CoxForm1  [B.CodePoint] (Maybe String)  String  (Cox c) -- ^ Form with single blank
    | CoxForm   [B.CodePoint] (Maybe String) [String] (Cox c) -- ^ Form with multiple blanks
    | CoxWith   [B.CodePoint] [NamedCox c] (Cox c)            -- ^ Cox with outside arguments

type CoxAssn c  = (B.BlankName, Cox c)
type NamedCox c = B.Named (Cox c)

instance B.CodePointer (Cox c) where
    codePoints (CoxLit    cp _)      =  cp
    codePoints (CoxTerm   cp _ _)    =  cp
    codePoints (CoxBase   cp _)      =  cp
    codePoints (CoxLocal  cp _ _)    =  cp
    codePoints (CoxBlank  cp _)      =  cp
    codePoints (CoxRefill cp _ _)    =  cp
    codePoints (CoxForm1  cp _ _ _)  =  cp
    codePoints (CoxForm   cp _ _ _)  =  cp
    codePoints (CoxWith   cp _ _)    =  cp

instance (B.Write c) => Show (Cox c) where
    show = show . B.doc

instance (B.Write c) => B.Write (Cox c) where
    write = docCox

docCox :: (B.Write c) => B.StringMap -> Cox c -> B.Doc
docCox sh = d (0 :: Int) . coxFold where
    wr           :: forall c. (B.Write c) => c -> B.Doc
    wrH, wrV     :: forall c. (B.Write c) => [c] -> B.Doc
    wr           = B.write  sh
    wrH          = B.writeH sh
    wrV          = B.writeV sh

    d 10 _ = wr "..."
    d n e  = case e of
        CoxLit    _ c          ->  wr "lit" B.<+> wr c
        CoxTerm   _ ns _       ->  wr $ concatMap ('/' :) ns
        CoxBase   _ cop        ->  wr "base" B.<+> wr (B.name cop)
        CoxLocal  _ v i        ->  wr "local" B.<+> wr v B.<> wr "/" B.<> wr i
        CoxBlank  _ v          ->  wr "global" B.<+> wr v
        CoxRefill _ f xs       ->  let f'  = wr ">>" B.<+> d' f
                                       xs' = B.nest 3 $ wrV $ map arg xs
                                   in f' B.$$ xs'
        CoxForm1  _ tag v  e2  ->  form tag [v] $ d' e2
        CoxForm   _ tag vs e2  ->  form tag vs  $ d' e2
        CoxWith   _ _ e2       ->  d' e2
      where
        d'                     =  d $ n + 1
        arg                    =  (wr "-" B.<+>) . d'
        form (Nothing)  vs     =  form2 vs
        form (Just tag) vs     =  form2 $ ("'" ++ tag) : vs
        form2 vs e2            =  B.docWraps "(|" "|)" $ wrH vs B.<+> wr "|" B.<+> e2
                                          
-- Convert CoxForm1 to CoxForm.
coxFold :: B.Map (Cox c)
coxFold (CoxForm1 cp1 tag1 v1 e1) =
    case coxFold e1 of
      CoxForm _ tag2 vs e2 | tag1 == tag2  ->  CoxForm cp1 tag1 (v1:vs) e2
      e2                                   ->  CoxForm cp1 tag1 [v1]    e2
coxFold (CoxRefill cp f xs) = CoxRefill cp (coxFold f) (map coxFold xs)
coxFold e = e

isCoxBase :: Cox c -> Bool
isCoxBase (CoxBase _ _)   = True
isCoxBase _               = False

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
      CoxBase _ _       ->  True
      CoxRefill _ f xs  ->  all irreducible $ f : xs
      _                 ->  False


-- ----------------------  Operator

-- | Content operator.
data Cop c
    = CopFun  B.BlankName (CopFun c)   -- ^ Convert contents
    | CopCox  B.BlankName (CopCox c)   -- ^ Convert coxes
    | CopTree B.BlankName (CopTree)    -- ^ Convert trees

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
    name = B.name . copName

copName :: Cop c -> B.BlankName
copName (CopFun   n _) = n
copName (CopCox   n _) = n
copName (CopTree  n _) = n

-- | Non-binary operator.
copNormal :: String -> B.BlankName
copNormal = B.BlankNormal

copInternal :: String -> B.BlankName
copInternal = B.BlankInternal

-- | Convert operator name to prefix name.
copPrefix :: String -> B.BlankName
copPrefix = B.BlankPrefix

-- | Convert operator name to postfix name.
copPostfix :: String -> B.BlankName
copPostfix = B.BlankPostfix

-- | Convert operator name to infix name.
copInfix :: String -> B.BlankName
copInfix = B.BlankInfix

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


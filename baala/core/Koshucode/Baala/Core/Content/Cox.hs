{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content expression.

module Koshucode.Baala.Core.Content.Cox
( -- $Process

  -- * Expression
  Cox (..), CopCalc, CoxTag, NamedCox,
  coxSyntacticArity,
  coxMap, coxCall,
  checkIrreducible,
) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Core.Message    as Message


-- ----------------------  Expression

-- | Term-content expressions.
data Cox c
    = CoxLit   [B.CodePt] c                        -- ^ Literal content
    | CoxTerm  [B.CodePt] [B.TermName] [Int]       -- ^ Term reference, its name and position
    | CoxCalc  [B.CodePt] B.BlankName (CopCalc c)  -- ^ Content calculator
    | CoxLocal [B.CodePt] String Int               -- ^ Local blank, its name and De Bruijn index
    | CoxBlank [B.CodePt] B.BlankName              -- ^ Blank in form
    | CoxFill  [B.CodePt] (Cox c) [Cox c]          -- ^ Fill args in a form
    | CoxForm1 [B.CodePt] CoxTag  String  (Cox c)  -- ^ Form with single blank
    | CoxForm  [B.CodePt] CoxTag [String] (Cox c)  -- ^ Form with multiple blanks
    | CoxWith  [B.CodePt] [NamedCox c] (Cox c)     -- ^ Cox with outside arguments

-- | Term-content calculator.
type CopCalc c = [B.Ab c] -> B.Ab c

type CoxTag = Maybe String

type NamedCox c = B.Named (Cox c)

instance B.CodePtr (Cox c) where
    codePts (CoxLit    cp _)      =  cp
    codePts (CoxTerm   cp _ _)    =  cp
    codePts (CoxCalc   cp _ _)    =  cp
    codePts (CoxLocal  cp _ _)    =  cp
    codePts (CoxBlank  cp _)      =  cp
    codePts (CoxFill   cp _ _)    =  cp
    codePts (CoxForm1  cp _ _ _)  =  cp
    codePts (CoxForm   cp _ _ _)  =  cp
    codePts (CoxWith   cp _ _)    =  cp

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
        CoxCalc   _ op _       ->  wr "calc" B.<+> wr op
        CoxLocal  _ v i        ->  wr "local" B.<+> wr v B.<> wr "/" B.<> wr i
        CoxBlank  _ v          ->  wr "global" B.<+> wr v
        CoxFill   _ f xs       ->  let f'  = wr ">>" B.<+> d' f
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
coxFold (CoxFill cp f xs) = CoxFill cp (coxFold f) (map coxFold xs)
coxFold e = e

isCoxForm :: Cox c -> Bool
isCoxForm (CoxForm1  _ _ _ _) = True
isCoxForm (CoxForm   _ _ _ _) = True
isCoxForm _                   = False

coxSyntacticArity :: Cox c -> Int
coxSyntacticArity = loop where
    loop (CoxForm   _ _ vs cox)  = loop cox + length vs
    loop (CoxForm1  _ _ _  cox)  = loop cox + 1
    loop (CoxFill _ cox xs)
        | isCoxForm cox  = loop cox - length xs
        | otherwise      = 0
    loop _ = 0

coxMap :: B.Map (B.Map (Cox c))
coxMap g (CoxFill  cp f xs)        = CoxFill  cp (g f) (map g xs)
coxMap g (CoxForm1 cp tag v  body) = CoxForm1 cp tag v  (g body)
coxMap g (CoxForm  cp tag vs body) = CoxForm  cp tag vs (g body)
coxMap _ e = e

coxCall :: Cox c -> (B.Map (Cox c)) -> Cox c
coxCall cox g = coxMap g cox

checkIrreducible :: B.AbMap (Cox c)
checkIrreducible e
    | irreducible e = Right e
    | otherwise     = Message.abCoxIrrep [e] $
                      Message.unkCox "Not irreducible"

-- irreducible representation
irreducible :: Cox c -> Bool
irreducible cox =
    case cox of
      CoxLit  _ _       ->  True
      CoxTerm _ _ _     ->  True
      CoxCalc _ _ _     ->  True
      CoxFill _ f xs    ->  all irreducible $ f : xs
      _                 ->  False




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


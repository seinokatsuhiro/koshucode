{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content expression.

module Koshucode.Baala.Data.Church.Cox
  ( -- * Data types
    Cox (..), Cox2, Cox3, Cox4,
    MaybeCox, NamedCox, CopCalc, CoxTag,
  
    -- * Functions
    coxLit,
    coxSyntacticArity,
    coxMap, coxCall,
    checkIrreducible,
  
    -- * Process
    -- $Process
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax               as S
import qualified Koshucode.Baala.Data.Church.Message  as Msg


-- ----------------------  Expression

-- | Term-content expressions.
data Cox c
    = CoxLit   [B.CodePt] c                        -- ^ Literal content
    | CoxTerm  [B.CodePt] [S.TermName] [Int]       -- ^ Term reference, its name and position
    | CoxCalc  [B.CodePt] S.BlankName (CopCalc c)  -- ^ Content calculator
    | CoxLocal [B.CodePt] String Int               -- ^ Local blank, its name and De Bruijn index
    | CoxBlank [B.CodePt] S.BlankName              -- ^ Blank in form
    | CoxFill  [B.CodePt] (Cox c) [Cox c]          -- ^ Fill args in a form
    | CoxForm1 [B.CodePt] CoxTag  String  (Cox c)  -- ^ Form with single blank
    | CoxForm  [B.CodePt] CoxTag [String] (Cox c)  -- ^ Form with multiple blanks
    | CoxWith  [B.CodePt] [NamedCox c] (Cox c)     -- ^ Cox with outside arguments

type Cox2 c = (Cox c, Cox c)
type Cox3 c = (Cox c, Cox c, Cox c)
type Cox4 c = (Cox c, Cox c, Cox c, Cox c)

type MaybeCox c = Maybe (Cox c)
type NamedCox c = B.Named (Cox c)

-- | Term-content calculator.
type CopCalc c = [B.Ab c] -> B.Ab c

type CoxTag = Maybe String

instance B.CodePtr (Cox c) where
    codePtList (CoxLit    cp _)       = cp
    codePtList (CoxTerm   cp _ _)     = cp
    codePtList (CoxCalc   cp _ _)     = cp
    codePtList (CoxLocal  cp _ _)     = cp
    codePtList (CoxBlank  cp _)       = cp
    codePtList (CoxFill   cp _ _)     = cp
    codePtList (CoxForm1  cp _ _ _)   = cp
    codePtList (CoxForm   cp _ _ _)   = cp
    codePtList (CoxWith   cp _ _)     = cp

instance (B.MixShortEncode c) => Show (Cox c) where
    show = show . coxToDoc B.noShorten

coxToDoc :: (B.MixShortEncode c) => B.Shorten -> Cox c -> B.Doc
coxToDoc sh = d (0 :: Int) . coxFold where
    wr         :: forall c. (B.Write c) => c -> B.Doc
    wrH, wrV   :: forall c. (B.Write c) => [c] -> B.Doc
    wr         = B.writeDocWith sh
    wrH        = B.writeH sh
    wrV        = B.writeV sh
    encode     = wr . B.mixToFlatString . B.mixShortEncode sh

    d 10 _ = wr "..."
    d n e  = case e of
        CoxLit    _ c          -> wr "lit" B.<+> encode c
        CoxTerm   _ ns _       -> wr $ concatMap ('/' :) ns
        CoxCalc   _ op _       -> wr "calc" B.<+> wr op
        CoxLocal  _ v i        -> wr "local" B.<+> wr v B.<> wr "/" B.<> wr i
        CoxBlank  _ v          -> wr "global" B.<+> wr v
        CoxFill   _ f xs       -> let f'  = wr ">>" B.<+> d' f
                                      xs' = B.nest 3 $ wrV $ map arg xs
                                  in f' B.$$ xs'
        CoxForm1  _ tag v  e2  -> form tag [v] $ d' e2
        CoxForm   _ tag vs e2  -> form tag vs  $ d' e2
        CoxWith   _ _ e2       -> d' e2
      where
        d'                      = d $ n + 1
        arg                     = (wr "-" B.<+>) . d'
        form (Nothing)  vs      = form2 vs
        form (Just tag) vs      = form2 $ ("'" ++ tag) : vs
        form2 vs e2             = B.docWraps "(|" "|)" $ wrH vs B.<+> wr "|" B.<+> e2

coxLit :: c -> Cox c
coxLit = CoxLit []

-- Convert CoxForm1 to CoxForm.
coxFold :: B.Map (Cox c)
coxFold (CoxForm1 cp1 tag1 v1 e1) =
    case coxFold e1 of
      CoxForm _ tag2 vs e2 | tag1 == tag2  ->  CoxForm cp1 tag1 (v1:vs) e2
      e2                                   ->  CoxForm cp1 tag1 [v1]    e2
coxFold (CoxFill cp f xs) = CoxFill cp (coxFold f) (map coxFold xs)
coxFold e = e

isCoxForm :: Cox c -> Bool
isCoxForm (CoxForm1  _ _ _ _)  = True
isCoxForm (CoxForm   _ _ _ _)  = True
isCoxForm _                    = False

coxSyntacticArity :: Cox c -> Int
coxSyntacticArity = loop where
    loop (CoxForm   _ _ vs cox)  = loop cox + length vs
    loop (CoxForm1  _ _ _  cox)  = loop cox + 1
    loop (CoxFill _ cox xs)
        | isCoxForm cox  = loop cox - length xs
        | otherwise      = 0
    loop _ = 0

coxMap :: B.Map (B.Map (Cox c))
coxMap g (CoxFill  cp f xs)         = CoxFill  cp (g f) (map g xs)
coxMap g (CoxForm1 cp tag v  body)  = CoxForm1 cp tag v  (g body)
coxMap g (CoxForm  cp tag vs body)  = CoxForm  cp tag vs (g body)
coxMap _ e = e

coxCall :: Cox c -> (B.Map (Cox c)) -> Cox c
coxCall cox g = coxMap g cox

checkIrreducible :: B.AbMap (Cox c)
checkIrreducible e
    | irreducible e  = Right e
    | otherwise      = Msg.abCoxIrrep [e] $
                       Msg.unkCox "Not irreducible"

-- irreducible representation
irreducible :: Cox c -> Bool
irreducible cox =
    case cox of
      CoxLit  _ _       -> True
      CoxTerm _ _ _     -> True
      CoxCalc _ _ _     -> True
      CoxFill _ f xs    -> all irreducible $ f : xs
      _                 -> False


-- ----------------------
-- $Process
--
--  /Phase 1/. Convert input string into token trees.
--
--    [1. 'String'@ -> \[@'B.Token'@\]@]
--        Parse input string into a list of tokens.
--
--    [2. @\[@'B.Token'@\] -> \[@'BTree.TTree'@\]@]
--        Analyze token list structure.
--
--    [3. @\[@'BTree.TTree'@\] -> @'BTree.TTree']
--        Wrap the list of token trees in a tree.
--
--  /Phase 2/. Transform token trees,
--             and convert into abstract syntax trees.
--             See "Koshucode.Baala.Data.Content.Build" module.
--
--    [4. 'BTree.TTree'@ -> @'BTree.TTree']
--       Expand tree-level syntax operators.
--
--    [5. 'BTree.TTree'@ -> @'BTree.TTree']
--       Transform binary operators from infix to prefix.
--
--    [6. 'BTree.TTree'@ -> @'Cox' @c@]
--       Convert token tree into an abstract syntax tree.
--
--    [7. 'Cox' @c -> @'Cox'@ c@]
--       Attach De Bruijn indecies for bound variables.
--
--    [8. 'Cox' @c -> @'Cox' @c@]
--       Expand expression-level syntax operators.
--
--  /Phase 3/. Calculate content expressions.
--             See "Koshucode.Baala.Data.Content.Run" module.
--
--    [9. 'B.Relhead'@ -> @'Cox'@ c -> @'Cox'@ c@]
--       Attach term positions using actural heading of relation.
--
--    [10. 'CopSet'@ c -> \[@'NamedCox'@ c\] -> @'Cox'@ c -> @'Cox'@ c@]
--       Replace base and derived operators into its bodies.
--
--    [11. 'Cox'@ c -> Beta c@]
--       Reduce derived expressions into beta expressions.
--
--    [12. @Beta c -> \[c\] -> c@]
--       Run beta expression for each association of relation.
--

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

import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax               as S
import qualified Koshucode.Baala.Data.Church.Message  as Msg


-- ----------------------  Expression

-- | Term-content expressions.
data Cox c
    = CoxLit   [B.CodePos] c
      -- ^ __1.__ Literal content
    | CoxTerm  [B.CodePos] [S.TermName] [S.TermIndex]
      -- ^ __2.__ Term reference, its name and index
    | CoxCalc  [B.CodePos] S.BlankName (CopCalc c)
      -- ^ __3.__ Content calculator
    | CoxLocal [B.CodePos] String Int
      -- ^ __4.__ Local blank, its name and De Bruijn index
    | CoxBlank [B.CodePos] S.BlankName
      -- ^ __5.__ Blank in form
    | CoxFill  [B.CodePos] (Cox c) [Cox c]
      -- ^ __6.__ Fill args in a form
    | CoxForm1 [B.CodePos] CoxTag  String  (Cox c)
      -- ^ __7.__ Form with single blank
    | CoxForm  [B.CodePos] CoxTag [String] (Cox c)
      -- ^ __8.__ Form with multiple blanks
    | CoxWith  [B.CodePos] [NamedCox c] (Cox c)
      -- ^ __9.__ Cox with outside arguments

-- | Expression pair.
type Cox2 c = (Cox c, Cox c)

-- | Expression triple.
type Cox3 c = (Cox c, Cox c, Cox c)

-- | Expression quadruple.
type Cox4 c = (Cox c, Cox c, Cox c, Cox c)

-- | Maybe expression.
type MaybeCox c = Maybe (Cox c)

-- | Named expression.
type NamedCox c = B.Named (Cox c)

-- | Term-content calculator.
type CopCalc c = [B.Ab c] -> B.Ab c

-- | Expression tag.
type CoxTag = Maybe String

instance B.GetCodePos (Cox c) where
    getCPs (CoxLit    cp _)       = cp
    getCPs (CoxTerm   cp _ _)     = cp
    getCPs (CoxCalc   cp _ _)     = cp
    getCPs (CoxLocal  cp _ _)     = cp
    getCPs (CoxBlank  cp _)       = cp
    getCPs (CoxFill   cp _ _)     = cp
    getCPs (CoxForm1  cp _ _ _)   = cp
    getCPs (CoxForm   cp _ _ _)   = cp
    getCPs (CoxWith   cp _ _)     = cp

instance (B.MixTransEncode c) => Show (Cox c) where
    show = show . coxToDoc O.nothing

coxToDoc :: (B.MixTransEncode c) => B.TransString -> Cox c -> B.Doc
coxToDoc sh = d (0 :: Int) . coxFold where
    encode = B.pprint . B.mixToFlatString . B.mixTransEncode sh

    d 10 _ = B.pprint "..."
    d n e  = case e of
        CoxLit    _ c          -> B.pprint "lit"    B.<+> encode c
        CoxTerm   _ ns _       -> B.pprint $ S.termPathString ns
        CoxCalc   _ op _       -> B.pprint "calc"   B.<+> blankNameToDoc op
        CoxLocal  _ v i        -> B.pprint "local"  B.<+> (B.pprint v O.++ B.pprint "/" O.++ B.pprint i)
        CoxBlank  _ v          -> B.pprint "global" B.<+> blankNameToDoc v
        CoxFill   _ f xs       -> let f'  = B.pprint ">>" B.<+> d' f
                                      xs' = B.nest 3 $ B.pprintV $ map arg xs
                                  in f' B.$$ xs'
        CoxForm1  _ tag v  e2  -> form tag [v] $ d' e2
        CoxForm   _ tag vs e2  -> form tag vs  $ d' e2
        CoxWith   _ _ e2       -> d' e2
      where
        d'                      = d $ n + 1
        arg                     = (B.pprint "-" B.<+>) . d'
        form (Nothing)  vs      = form2 vs
        form (Just tag) vs      = form2 $ ("'" ++ tag) : vs
        form2 vs e2             = B.pprintWraps "(|" "|)" $ B.pprintH vs B.<+> B.pprint "|" B.<+> e2

blankNameToDoc :: S.BlankName -> B.Doc
blankNameToDoc (S.BlankNormal   n) = B.pprint n
blankNameToDoc (S.BlankInternal n) = B.pprint n
blankNameToDoc (S.BlankPrefix   n) = B.pprint n B.<+> B.pprint "(prefix)"
blankNameToDoc (S.BlankInfix    n) = B.pprint n B.<+> B.pprint "(infix)"
blankNameToDoc (S.BlankPostfix  n) = B.pprint n B.<+> B.pprint "(postfix)"

-- | Create literal expression.
coxLit :: c -> Cox c
coxLit = CoxLit []

-- Convert CoxForm1 to CoxForm.
coxFold :: O.Map (Cox c)
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

-- | Get arity of expression.
coxSyntacticArity :: Cox c -> Int
coxSyntacticArity = loop where
    loop (CoxForm   _ _ vs cox)  = loop cox + length vs
    loop (CoxForm1  _ _ _  cox)  = loop cox + 1
    loop (CoxFill _ cox xs)
        | isCoxForm cox  = loop cox - length xs
        | otherwise      = 0
    loop _ = 0

-- | Map expression form.
coxMap :: O.Map (O.Map (Cox c))
coxMap g (CoxFill  cp f xs)         = CoxFill  cp (g f) (map g xs)
coxMap g (CoxForm1 cp tag v  body)  = CoxForm1 cp tag v  (g body)
coxMap g (CoxForm  cp tag vs body)  = CoxForm  cp tag vs (g body)
coxMap _ e = e

-- | Fill expression form.
coxCall :: Cox c -> (O.Map (Cox c)) -> Cox c
coxCall cox g = coxMap g cox

-- | Check irreducible expression.
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

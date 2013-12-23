{-# OPTIONS_GHC -Wall #-}

{-| Term calcutation. -}

module Koshucode.Baala.Core.Content.Expression
(
  -- $Process
  formCox,
  PosCox,
  posCox,
  runCox,
  runCoxH,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content.Class     as C
import qualified Koshucode.Baala.Core.Content.Extension as C
import qualified Koshucode.Baala.Core.Content.Operator  as C



{-| Construct content expression. -}
formCox
    :: (C.CContent c)
    => C.FindCop c      -- ^ Collection of operators
    -> B.TokenTree      -- ^ Input token tree
    -> B.Ab (C.Cox c)   -- ^ Result content expression
formCox cops = form where
    form x@(B.TreeL tok) = case tok of
        B.TWord _ _ _  ->  fmap C.CoxLit $ C.litContent x
        B.TTerm _ ns   ->  Right $ C.CoxTerm ns []
        _              ->  B.bug

    -- parend unquoted word and its arguments
    form (B.TreeB 1 (B.TreeL (B.TWord _ 0 w) : args)) =
        case cops w of
          Just cop   ->  call cop args
          Nothing    ->  Left $ B.AbortAnalysis [] $ B.AAUnkCop w
    form x@(B.TreeB n _)
        | n >  1      =  fmap C.CoxLit $ C.litContent x  -- literal composite
    form x            =  Left $ B.AbortSyntax [] $ B.ASUnkCox (show x) -- unknown

    call (C.CopLit _ f) = fmap C.CoxLit . f
    call op'            = fmap (C.CoxApp op') . mapM form

type PosCox c = B.Relhead -> B.Ab (C.Cox c)

{-| Put term positions for actural heading. -}
posCox :: C.Cox c -> PosCox c
posCox cox h = pos cox where
    pos (C.CoxApp f cs)  = Right . C.CoxApp f =<< mapM pos cs
    pos (C.CoxTerm ns _) = let index = B.headIndex1 h ns
                         in if all (>= 0) index
                            then Right $ C.CoxTerm ns index
                            else Left  $ B.AbortAnalysis [] (B.AANoTerms [concat ns])
    pos c = Right c

runCoxH
  :: (C.CRel c, C.CList c)
  => B.Relhead    -- ^ Heading of relation
  -> [c]          -- ^ Tuple in body of relation
  -> (PosCox c)   -- ^ Content expression
  -> B.Ab c       -- ^ Calculated literal content
runCoxH h lits cox =
    runCox lits =<< cox h

{-| Calculate content expression. -}
runCox :: (C.CList c, C.CRel c) => [c] -> C.Cox c -> B.Ab c
runCox arg cox = run cox where
    run (C.CoxLit c)      =   Right c
    run (C.CoxTerm _ [p]) =   Right $ arg !! p
    run (C.CoxTerm _ ps)  =   term ps arg
    run (C.CoxApp op cs)  =
        case op of
          C.CopLazy  _ f   ->  f cs
          C.CopEager _ f   ->  f =<< mapM run cs
          C.CopLit   _ _   ->  Left $ B.abortNotFound ""

    term []     _ = Left $ B.abortNotFound ""
    term (-1:_) _ = Left $ B.abortNotFound ""
    term (p:ps) arg2 =
        let c = arg2 !! p
        in if C.isRel c
           then rel ps $ C.getRel c
           else Right c

    rel ps (B.Rel _ args) =
        Right . C.putList =<< mapM (term ps) args

{-
let op _ = Right $ CopEager "+" f where f xs = Right $ foldr (+) 0 xs
formCox op [] $ treeG . tokenTrees $ tokens "(+ (int 1) (int 2))"
let e1 = CoxApp plus [CoxLit 1, CoxLit 2, CoxTerm ["/?"] [0]]
runCox e1 [1,2,3]
-}

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
        See 'B.treeG'.

     [4. @Tree Token -> Tree Token@]
        Translate binary operators from infix to prefix.
        See 'B.binaryTree'.

   Phase 2. From prefixed token tree to literal content.

     [5. @Tree Token -> Cox c@]
        Convert from token tree to content expression.
        See 'formCox'.

     [6. @Cox c -> (Relhead -> Cox c)@]
        Attach term positions using actural heading of relation.
        See 'posCox'.

     [7. @Cox c -> \[c\] -> c@]
        Calculate content expression for each tuple of relation.
        See 'runCox'.

-}


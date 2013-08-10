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
import Koshucode.Baala.Core.Content.Class
import Koshucode.Baala.Core.Content.Extension
import Koshucode.Baala.Core.Content.Operator



{-| Construct content expression. -}
formCox
    :: (CContent c)
    => FindCop c      -- ^ Collection of operators
    -> B.TokenTree    -- ^ Input token tree
    -> B.Ab (Cox c)   -- ^ Result content expression
formCox cops = form where
    form x@(B.TreeL tok) = case tok of
        B.TWord _ _ _  ->  fmap CoxLit $ litContent x
        B.TTerm _ ns   ->  Right $ CoxTerm ns []
        _              ->  B.bug

    -- parend unquoted word and its arguments
    form (B.TreeB 1 (B.TreeL (B.TWord _ 0 w) : args)) =
        case cops w of
          Just cop   ->  call cop args
          Nothing    ->  Left $ B.AbortUnkCop w
    form x@(B.TreeB n _)
        | n >  1      =  fmap CoxLit $ litContent x  -- literal composite
    form x            =  Left $ B.AbortUnkCox (show x) -- unknown

    call (CopLit _ f) = fmap CoxLit . f
    call op'          = fmap (CoxApp op') . mapM form

type PosCox c = B.Relhead -> B.Ab (Cox c)

{-| Put term positions for actural heading. -}
posCox :: Cox c -> PosCox c
posCox cox h = pos cox where
    pos (CoxApp f cs)  = Right . CoxApp f =<< mapM pos cs
    pos (CoxTerm ns _) = let index = B.headIndex1 h ns
                         in if all (>= 0) index
                            then Right $ CoxTerm ns index
                            else Left  $ B.AbortNoTerms [concat ns]
    pos c = Right c

runCoxH
  :: (CRel c, CList c)
  => B.Relhead    -- ^ Heading of relation
  -> [c]          -- ^ Tuple in body of relation
  -> (PosCox c)   -- ^ Content expression
  -> B.Ab c       -- ^ Calculated literal content
runCoxH h lits cox =
    runCox lits =<< cox h

{-| Calculate content expression. -}
runCox :: (CList c, CRel c) => [c] -> Cox c -> B.Ab c
runCox arg cox = run cox where
    run (CoxLit c)      =   Right c
    run (CoxTerm _ [p]) =   Right $ arg !! p
    run (CoxTerm _ ps)  =   term ps arg
    run (CoxApp op cs)  =
        case op of
          CopLazy  _ f   ->  f cs
          CopEager _ f   ->  f =<< mapM run cs
          CopLit   _ _   ->  Left $ B.AbortLookup ""

    term []     _ = Left $ B.AbortLookup ""
    term (-1:_) _ = Left $ B.AbortLookup ""
    term (p:ps) arg2 =
        let c = arg2 !! p
        in if isRel c
           then rel ps $ getRel c
           else Right c

    rel ps (B.Rel _ args) =
        Right . putList =<< mapM (term ps) args

{-
let op _ = Right $ CopEager "+" f where f xs = Right $ foldr (+) 0 xs
formCox op [] $ singleTree . tokenTrees $ tokens "(+ (int 1) (int 2))"
let e1 = CoxApp plus [CoxLit 1, CoxLit 2, CoxTerm ["/?"] [0]]
runCox e1 [1,2,3]
-}

-- ----------------------
{- $Process

   Phase 1. From string to prefixed token tree.

     [1. @String -> \[Token\]@]
        Parse input string into list of token.
        See 'tokens'.

     [2. @\[Token\] -> \[Tree Token\]@]
        Analyze token list structure.
        See 'tokenTrees'

     [3. @\[Tree Token\] -> Tree Token@]
        Enclose list of token tree in 'TreeB'.
        See 'singleTree'.

     [4. @Tree Token -> Tree Token@]
        Translate binary operators from infix to prefix.
        See 'binaryTree'.

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


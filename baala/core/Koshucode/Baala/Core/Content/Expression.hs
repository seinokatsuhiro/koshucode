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

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Core.Content.Class
import Koshucode.Baala.Core.Content.Extension
import Koshucode.Baala.Core.Content.Operator



{-| Construct content expression. -}
formCox
    :: (CContent c)
    => FindCop c     -- ^ Collection of operators
    -> TokenTree     -- ^ Input token tree
    -> AbOr (Cox c)  -- ^ Result content expression
formCox op = form where
    form x@(TreeL (TWord _ _ _)) = fmap CoxLit $ litContent x
    form (TreeL (TTerm _ ns))    = Right $ CoxTerm ns []
    form (TreeL _)               = Left  $ AbortLookup ""
    form (TreeB _ (TreeL (TWord _ _ n) : xs)) =
        case op n of
          Just op'      ->  call xs op'
          Nothing       ->  Left $ AbortUnkCop n
    form (TreeB _ [x])  =   form x
    form (TreeB _ _)    =   Left $ AbortLookup ""

    call xs (CopLit _ f) = fmap CoxLit $ f xs
    call xs op' = Right . CoxApp op' =<< mapM form xs

type PosCox c = Relhead -> AbOr (Cox c)

{-| Put term positions for actural heading. -}
posCox :: Cox c -> PosCox c
posCox cox h = pos cox where
    pos (CoxApp f cs)  = Right . CoxApp f =<< mapM pos cs
    pos (CoxTerm ns _) = let index = headIndex1 h ns
                          in if all (>= 0) index
                             then Right $ CoxTerm ns index
                             else Left  $ AbortNoTerm (concat ns)
    pos c = Right c

runCoxH
  :: (CRel c, CList c)
  => Relhead       -- ^ Heading of relation
  -> [c]           -- ^ Tuple in body of relation
  -> (PosCox c)    -- ^ Content expression
  -> AbOr c        -- ^ Calculated literal content
runCoxH h lits cox =
    runCox lits =<< cox h

{-| Calculate content expression. -}
runCox :: (CList c, CRel c) => [c] -> Cox c -> AbOr c
runCox arg cox = run cox where
    run (CoxLit c)      =   Right c
    run (CoxTerm _ [p]) =   Right $ arg !! p
    run (CoxTerm _ ps)  =   term ps arg
    run (CoxApp op cs)  =
        case op of
          CopLazy  _ f   ->  f cs
          CopEager _ f   ->  f =<< mapM run cs
          CopLit   _ _   ->  Left $ AbortLookup ""

    term []     _ = Left $ AbortLookup ""
    term (-1:_) _ = Left $ AbortLookup ""
    term (p:ps) arg2 =
        let c = arg2 !! p
        in if isRel c
           then rel ps $ getRel c
           else Right c

    rel ps (Rel _ args) =
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


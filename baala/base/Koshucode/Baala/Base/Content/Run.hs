{-# OPTIONS_GHC -Wall #-}

{-| Term calcutation. -}

module Koshucode.Baala.Base.Content.Run
(
  -- * Operator
  Cop (..),
  CopLitF,
  CopLazyF,
  CopEagerF,
  FindCop,
  namedLit,
  namedEager,
  namedLazy,

  -- * Expression
  -- $Process
  ContExp (..),
  formContent,
  PosContent,
  posContent,
  runContent,
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Base.Content.Class
import Koshucode.Baala.Base.Content.Literalize



-- ----------------------  Operator

{-| Term content operator. -}
data Cop c
    = CopLit   String (CopLitF   c)
    | CopLazy  String (CopLazyF  c)
    | CopEager String (CopEagerF c)

instance Show (Cop c) where
    show (CopLit   n _) = "(CopLit "   ++ show n ++ " _)"
    show (CopEager n _) = "(CopEager " ++ show n ++ " _)"
    show (CopLazy  n _) = "(CopLazy "  ++ show n ++ " _)"

type CopLitF   c = [TokenTree] -> AbOr c
type CopLazyF  c = [ContExp c] -> AbOr c
type CopEagerF c = [c]         -> AbOr c

{-| Type for finding term content operator. -}
type FindCop c = String -> Maybe (Cop c)

namedLit :: String -> CopLitF c -> (String, Cop c)
namedLit n f = (n, CopLit n f)

namedEager :: String -> CopEagerF c -> (String, Cop c)
namedEager n f = (n, CopEager n f)

namedLazy :: String -> CopLazyF c -> (String, Cop c)
namedLazy n f = (n, CopLazy n f)



-- ----------------------  Content

data ContExp c
    {-| Literal content. -}
    = ContLit c

    {-| Operator invocation. -}
    | ContApp  (Cop c) [ContExp c]

    {-| Term reference.
        Term names @[String]@ and positions @[Int]@.
        'formContent' makes positions empty,
        'posContent' fills it. -}
    | ContTerm [String] [Int]

    deriving (Show)

{-| Construct content expression. -}
formContent
    :: (CContent c)
    => FindCop c           -- ^ Collection of operators
    -> [SourceLine]        -- ^ Source code information
    -> TokenTree           -- ^ Input token tree
    -> AbortOr (ContExp c) -- ^ Result content expression
formContent op src = form where
    form x@(TreeL (TWord _ _ _)) = do lit <- litContent src x
                                      Right $ ContLit lit
    form (TreeL (TTerm _ ns))    = Right $ ContTerm ns []
    form (TreeL _)               = Left (AbortLookup "", src)
    form (TreeB _ (TreeL (TWord _ _ n) : xs)) =
        case op n of
          Just op'      ->  call xs op'
          Nothing       ->  Left (AbortUnkCop n, src)
    form (TreeB _ [x])  =   form x
    form (TreeB _ _)    =   Left (AbortLookup "", src)

    call xs (CopLit _ f) =
        case f xs of
          Right lit     ->  Right $ ContLit lit
          Left  a       ->  Left (a, src)
    call xs op' =
        do xs' <- mapM form xs
           Right $ ContApp op' xs'

type PosContent c = Relhead -> ContExp c

{-| Put term positions for actural heading. -}
posContent :: ContExp c -> PosContent c
posContent cont h = pos cont where
    pos (ContTerm ns _) = ContTerm ns $ termLook1 ns (headTerms h)
    pos (ContApp f cs)  = ContApp f $ map pos cs
    pos c@(ContLit _)   = c

{-| Calculate content expression. -}
runContent :: (CList c, CRel c) => ContExp c -> [c] -> AbOr c
runContent cont arg = run cont where
    run (ContLit c)      =   Right c
    run (ContTerm _ [p]) =   Right $ arg !! p
    run (ContTerm _ ps)  =   term ps arg
    run (ContApp op cs)  =
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

    rel ps (Rel _ args) = do args' <- mapM (term ps) args
                             Right . putList $ args'

{-
let op _ = Right $ CopEager "+" f where f xs = Right $ foldr (+) 0 xs
formContent op [] $ singleTree . tokenTrees $ tokens "(+ (int 1) (int 2))"
let e1 = ContApp plus [ContLit 1, ContLit 2, ContTerm ["/?"] [0]]
runContent e1 [1,2,3]
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

     [5. @Tree Token -> ContExp c@]
        Convert from token tree to content expression.
        See 'formContent'.

     [6. @ContExp c -> (Relhead -> ContExp c)@]
        Attach term positions using actural heading of relation.
        See 'posContent'.

     [7. @ContExp c -> \[c\] -> c@]
        Calculate content expression for each tuple of relation.
        See 'runContent'.

-}


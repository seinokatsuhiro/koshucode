{-# OPTIONS_GHC -Wall #-}

{-| Literalizer: Make literal contents from token tree. -}

module Koshucode.Baala.Base.Content.Run
(
-- * Operator
  ContentOp (..),
  CopLitF,
  CopLazyF,
  CopEagerF,
  namedLit,
  namedEager,
  namedLazy,

-- * Content
  Content (..),
  PosContent,
  formContent,
  posContent,
  runContent,
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Base.Content.Class
import Koshucode.Baala.Base.Content.Literalize



-- ----------------------  Operator

type CopLitF   c = [TokenTree] -> AbOr c
type CopLazyF  c = [Content c] -> AbOr c
type CopEagerF c = [c]         -> AbOr c

data ContentOp c
    = CopLit   String (CopLitF   c)
    | CopLazy  String (CopLazyF  c)
    | CopEager String (CopEagerF c)

instance Show (ContentOp c) where
    show (CopLit   n _) = "(CopLit "   ++ show n ++ " _)"
    show (CopEager n _) = "(CopEager " ++ show n ++ " _)"
    show (CopLazy  n _) = "(CopLazy "  ++ show n ++ " _)"

namedLit :: String -> CopLitF c -> (String, ContentOp c)
namedLit n f = (n, CopLit n f)

namedEager :: String -> CopEagerF c -> (String, ContentOp c)
namedEager n f = (n, CopEager n f)

namedLazy :: String -> CopLazyF c -> (String, ContentOp c)
namedLazy n f = (n, CopLazy n f)



-- ----------------------  Content

data Content c
    = ContLit c                           -- ^ Literal content
    | ContApp  (ContentOp c) [Content c]  -- ^ Operator invocation
    | ContTerm [String] [Int]             -- ^ Term reference
      deriving (Show)

type PosContent c = Relhead -> Content c

formContent
    :: (CContent c)
    => (String -> Maybe (ContentOp c))
    -> [SourceLine]
    -> TokenTree
    -> AbortOr (Content c)
formContent op src = form where
    form x@(TreeL (TWord _ _ _)) = do lit <- litContent src x
                                      Right $ ContLit lit
    form (TreeL (TTermN _ ns))   = Right $ ContTerm ns []
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

{-| Put term positions for actural heading. -}
posContent :: Content c -> PosContent c
posContent cont h = pos cont where
    pos (ContTerm ns _) = ContTerm ns $ termLook1 ns (headTerms h)
    pos (ContApp f cs)  = ContApp f $ map pos cs
    pos c@(ContLit _)   = c

runContent :: (CList c, CRel c) => Content c -> [c] -> AbOr c
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
formContent op [] $ singleToken . tokenTrees $ tokens "(+ (int 1) (int 2))"
let e1 = ContApp plus [ContLit 1, ContLit 2, ContTerm ["/?"] [0]]
runContent e1 [1,2,3]
-}


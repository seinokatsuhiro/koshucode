{-# OPTIONS_GHC -Wall #-}

{-| Literalizer: Make literal contents from token tree. -}

module Koshucode.Baala.Base.Content.Run
(
-- * Library
  Content (..),
  ContentOp (..),
  namedLit,
  namedEager,
  namedLazy,
  formContent,
  runContent,
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Base.Content.Class
import Koshucode.Baala.Base.Content.Literalize

data Content c
    = ContLit c
    | ContApp  (ContentOp c) [Content c]
    | ContTerm [String] [Int]
      deriving (Show)



-- ----------------------

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



-- ----------------------

formContent
    :: (Value c)
    => (String -> Maybe (ContentOp c))
    -> [SourceLine]
    -> TokenTree
    -> AbortOr (Content c)
formContent op src = form where
    form x@(TreeL (TWord _ _ _)) = do lit <- litContent src x
                                      Right $ ContLit lit
    form (TreeL (TTermN _ ns))   = Right $ ContTerm ns []
    form (TreeL _)               = Left $ AbortLookup src ""
    form (TreeB _ (TreeL (TWord _ _ n) : xs)) =
        case op n of
          Just op'  ->  call xs op'
          Nothing   ->  Left $ AbortLookup src n
    form (TreeB _ [x]) = form x
    form (TreeB _ _)   = Left $ AbortLookup src ""

    call xs (CopLit _ f) =
        case f xs of
          Right lit  ->  Right $ ContLit lit
          Left  ab   ->  Left $ ab src
    call xs op' =
        do xs' <- mapM form xs
           Right $ ContApp op' xs'

runContent :: Content c -> [c] -> AbOr c
runContent cont xs = run cont where
    run (ContLit c)      =  Right c
    run (ContTerm _ [n]) =  Right $ xs !! n
    run (ContTerm _ _)   =  Left $ \src -> AbortLookup src ""
    run (ContApp op cs)  =
        case op of
          CopLazy  _ f   ->  f cs
          CopEager _ f   ->  f =<< mapM run cs
          CopLit   _ _   ->  Left $ \src -> AbortLookup src ""

{-


let op _ = Right $ CopEager "+" f where f xs = Right $ foldr (+) 0 xs
formContent op [] $ singleToken . tokenTrees $ tokens "(+ (int 1) (int 2))"
let e1 = ContApp plus [ContLit 1, ContLit 2, ContTerm ["/?"] [0]]
runContent e1 [1,2,3]

-}


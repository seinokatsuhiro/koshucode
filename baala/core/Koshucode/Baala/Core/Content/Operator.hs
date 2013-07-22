{-# OPTIONS_GHC -Wall #-}

{-| Term calcutation. -}

module Koshucode.Baala.Core.Content.Operator
(
  Cox (..),
  Cop (..),
  CopLitF,
  CopLazyF,
  CopEagerF,
  FindCop,
  namedLit,
  namedEager,
  namedLazy,

) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Syntax


{-| Content expressions. -}
data Cox c
    {-| Literal content. -}
    = CoxLit c

    {-| Operator invocation. -}
    | CoxApp  (Cop c) [Cox c]

    {-| Term reference.
        Term names @[String]@ and positions @[Int]@.
        'formCox' makes positions empty,
        'posCox' fills it. -}
    | CoxTerm [String] [Int]

    deriving (Show)

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
type CopLazyF  c = [Cox c]     -> AbOr c
type CopEagerF c = [c]         -> AbOr c

{-| Type for finding term content operator. -}
type FindCop c = String -> Maybe (Cop c)

namedLit :: String -> CopLitF c -> (String, Cop c)
namedLit n f = (n, CopLit n f)

namedEager :: String -> CopEagerF c -> (String, Cop c)
namedEager n f = (n, CopEager n f)

namedLazy :: String -> CopLazyF c -> (String, Cop c)
namedLazy n f = (n, CopLazy n f)

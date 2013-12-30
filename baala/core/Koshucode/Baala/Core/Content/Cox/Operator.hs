{-# OPTIONS_GHC -Wall #-}

{-| Term calcutation. -}

module Koshucode.Baala.Core.Content.Cox.Operator
(
  Cox,
  CoxCore (..),
  Cop (..),
  CopLitF,
  CopLazyF,
  CopEagerF,
  FindCop,
  namedLit,
  namedEager,
  namedLazy,

) where

import qualified Koshucode.Baala.Base as B

type Cox c = B.Sourced (CoxCore c)

{-| Content expressions. -}
data CoxCore c
    {-| Literal content. -}
    = CoxLit c

    {-| Operator invocation. -}
    | CoxApp (Cop c) [Cox c]

    {-| Term reference.
        Term names @[String]@ and positions @[Int]@.
        'coxCons' makes positions empty,
        'coxPos' fills it. -}
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

type CopLitF   c = [B.TokenTree] -> B.Ab c
type CopLazyF  c = [Cox c]       -> B.Ab c
type CopEagerF c = [c]           -> B.Ab c

{-| Type for finding term content operator. -}
type FindCop c = String -> Maybe (Cop c)

namedLit :: String -> CopLitF c -> (String, Cop c)
namedLit n f = (n, CopLit n f)

namedEager :: String -> CopEagerF c -> (String, Cop c)
namedEager n f = (n, CopEager n f)

namedLazy :: String -> CopLazyF c -> (String, Cop c)
namedLazy n f = (n, CopLazy n f)

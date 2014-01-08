{-# OPTIONS_GHC -Wall #-}

{-| Term calcutation. -}

module Koshucode.Baala.Core.Content.Cox.Operator
(
  Cox,
  CoxCore (..),
  Cop (..),
  CopLitF, CopFunF, CopMacroF,
  FindCop,
  coxLit, copFun, copMacro,
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

{-| Term-content operator. -}
data Cop c
    = CopLit   String (CopLitF   c)
    | CopMacro String (CopMacroF c)
    | CopFun   String (CopFunF   c)

instance Show (Cop c) where
    show (CopLit   n _)  = "(CopLit "   ++ show n ++ " _)"
    show (CopFun n _)    = "(CopFun "   ++ show n ++ " _)"
    show (CopMacro  n _) = "(CopMacro "  ++ show n ++ " _)"

instance B.Name (Cop c) where
    name (CopLit   n _)  = n
    name (CopFun n _)    = n
    name (CopMacro  n _) = n

type CopLitF   c = [B.TokenTree] -> B.Ab c
type CopFunF   c = [c]           -> B.Ab c
type CopMacroF c = [Cox c]       -> B.Ab (Cox c)

{-| Type for finding term content operator. -}
type FindCop c = String -> Maybe (Cop c)

coxLit :: String -> CopLitF c -> (String, Cop c)
coxLit n f = (n, CopLit n f)

copFun :: String -> CopFunF c -> (String, Cop c)
copFun n f = (n, CopFun n f)

copMacro :: String -> CopMacroF c -> (String, Cop c)
copMacro n f = (n, CopMacro n f)


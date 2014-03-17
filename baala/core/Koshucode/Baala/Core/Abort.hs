{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Abort
( ambInfixes,
  noFile,
  oddRelation,
  unexpOperand,
  unkClause,
  unkCox,
  unkWord,
  unresPrefix,
) where

import qualified Koshucode.Baala.Base as B


-- ----------------------  Function

ab :: AbortCore -> B.Ab a
ab = Left . B.abortBy

ambInfixes :: [String] -> B.Ab a
ambInfixes = ab . AmbInfixes

-- | File not found
noFile :: String -> B.Ab a
noFile = ab . NoFile

-- | Odd relation literal
oddRelation :: B.Ab a
oddRelation = ab OddRelation

-- | Unexpected term names
unexpOperand :: String -> B.Ab a
unexpOperand = ab . UnexpOperand

-- | Unknown clause
unkClause :: B.Ab a
unkClause = ab UnkClause

-- | Unknown expression
unkCox :: String -> B.Ab a
unkCox = ab . UnkCox

-- | Unknown word
unkWord :: String -> B.Ab a
unkWord = ab . UnkWord

-- | Unresolved prefix
unresPrefix :: B.Ab a
unresPrefix = ab UnresPrefix


-- ----------------------  Datatype

data AbortCore
    = AmbInfixes [String]
    | NoFile String
    | OddRelation
    | UnexpOperand String
    | UnkClause
    | UnkCox String
    | UnkWord String
    | UnresPrefix
      deriving (Show, Eq, Ord)

instance B.AbortBy AbortCore where
    abortBy a = B.AbortReason
                { B.abortSymbol = B.abortSymbolGet a
                , B.abortReason = r a
                , B.abortDetail = d a
                , B.abortSource = []
                } where

        r (AmbInfixes _)     = "Ambiguous infix operators"
        r (NoFile _)         = "File not found"
        r (OddRelation)      = "Odd relation literal"
        r (UnexpOperand _)   = "Unexpected term names"
        r (UnkClause)        = "Unknown clause"
        r (UnkCox _)         = "Unknown expression"
        r (UnkWord _)        = "Unknown word"
        r (UnresPrefix)      = "Unresolved prefix"

        d (AmbInfixes ops)   = ops
        d (NoFile path)      = [path]
        d (UnkCox s)         = [s]
        d (UnkWord s)        = [s]
        d (UnexpOperand s)   = [s]
        d _                  = []


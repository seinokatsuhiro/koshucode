{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Abort
( adlib,
  ambInfixes,
  checkTerm,
  noFile,
  noTerm,
  oddRelation,
  reqFlatname,
  reqTermName,
  unexpOperand,
  unkClause,
  unkCop,
  unkCox,
  unkRelmap,
  unkWord,
  unresPrefix,
) where

import qualified Koshucode.Baala.Base as B


-- ----------------------  Function

ab :: AbortCore -> B.Ab a
ab = Left . B.abortBy

-- | AD-LIB: reason
adlib :: String -> B.Ab a
adlib reason = Left $ B.abortBecause $ "AD-LIB: " ++ reason

-- | Ambiguous infix operators
ambInfixes :: [String] -> B.Ab a
ambInfixes = Left . B.abortLines "Ambiguous infix operators"

-- | File not found
checkTerm :: [String] -> B.Ab a
checkTerm = Left . B.abortLines "check-term failed"

-- | File not found
noFile :: String -> B.Ab a
noFile = ab . NoFile

-- | Input relation does not given terms
noTerm :: [String] -> B.Ab a
noTerm = Left . B.abortLines "Input relation does not given terms"

-- | Odd relation literal
oddRelation :: B.Ab a
oddRelation = ab OddRelation

-- | Require flatname
reqFlatname :: String -> B.Ab a
reqFlatname = Left . B.abortLine "Require flatname"

reqTermName :: B.Ab a
reqTermName = Left $ B.abortBecause "Require term name"

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

-- | Unknown content operator
unkCop :: String -> B.Ab a
unkCop = Left . B.abortLine "Unknown content operator"

-- | Unknown relmap operator
unkRelmap :: String -> B.Ab a
unkRelmap = Left . B.abortLine "Unknown relmap operator"

-- | Unresolved prefix
unresPrefix :: B.Ab a
unresPrefix = ab UnresPrefix



-- ----------------------  Datatype

data AbortCore
    = NoFile String
    | OddRelation
    | UnexpOperand String
    | UnkClause
    | UnkCox String
    | UnkWord String
    | UnresPrefix
      deriving (Show, Eq, Ord)

instance B.AbortBy AbortCore where
    abortBy a = B.AbortReason
                { B.abortReason = r a
                , B.abortDetail = d a
                , B.abortSource = []
                } where

        r (NoFile _)         = "File not found"
        r (OddRelation)      = "Odd relation literal"
        r (UnexpOperand _)   = "Unexpected term names"
        r (UnkClause)        = "Unknown clause"
        r (UnkCox _)         = "Unknown expression"
        r (UnkWord _)        = "Unknown word"
        r (UnresPrefix)      = "Unresolved prefix"

        d (NoFile path)      = [path]
        d (UnkCox s)         = [s]
        d (UnkWord s)        = [s]
        d (UnexpOperand s)   = [s]
        d _                  = []


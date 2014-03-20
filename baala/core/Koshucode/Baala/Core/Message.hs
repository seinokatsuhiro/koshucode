{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Message
( -- * Base package
  module Koshucode.Baala.Base.Message,

  -- * Core package
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
  unmatchType,
  unresPrefix,
) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Base.Message


-- | Ambiguous infix operators
ambInfixes :: [String] -> B.Ab a
ambInfixes = Left . B.abortLines "Ambiguous infix operators"

-- | File not found
checkTerm :: [String] -> B.Ab a
checkTerm = Left . B.abortLines "check-term failed"

-- | File not found
noFile :: String -> B.Ab a
noFile = Left . B.abortLine "File not found"

-- | No term in relation
noTerm :: [String] -> B.Ab a
noTerm = Left . B.abortLines "No term in relation"

-- | Odd relation literal
oddRelation :: B.Ab a
oddRelation = Left $ B.abortBecause "Odd relation literal"

-- | Require flatname
reqFlatname :: String -> B.Ab a
reqFlatname = Left . B.abortLine "Require flatname"

reqTermName :: B.Ab a
reqTermName = Left $ B.abortBecause "Require term name"

-- | Unexpected operand
unexpOperand :: String -> B.Ab a
unexpOperand = Left . B.abortLine "Unexpected operand"

-- | Unknown clause
unkClause :: B.Ab a
unkClause = Left $ B.abortBecause "Unknown clause"

-- | Unknown expression
unkCox :: String -> B.Ab a
unkCox = Left . B.abortLine "Unknown expression"

-- | Unknown word
unkWord :: String -> B.Ab a
unkWord = Left . B.abortLine "Unknown word"

-- | Unknown content operator
unkCop :: String -> B.Ab a
unkCop = Left . B.abortLine "Unknown content operator"

-- | Unknown relmap operator
unkRelmap :: String -> B.Ab a
unkRelmap = Left . B.abortLine "Unknown relmap operator"

unmatchType :: String -> B.Ab a
unmatchType = Left . B.abortLine "Type unmatch"

-- | Unresolved prefix
unresPrefix :: B.Ab a
unresPrefix = Left $ B.abortBecause "Unresolved prefix"


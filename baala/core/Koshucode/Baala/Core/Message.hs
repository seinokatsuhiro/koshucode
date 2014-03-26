{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Message
( -- * Base package
  module Koshucode.Baala.Base.Message,

  -- * Core package
  ambInfixes,
  noFile,
  oddRelation,
  reqFlatName,
  reqTermName,
  unexpOperand,
  unkClause,
  unkCop,
  unkCox,
  unkRelmap,
  unkTerm,
  unkWord,
  unmatchType,
  unresPrefix,

  -- * Utility
  detailTermRel,
) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Base.Message


-- | Ambiguous infix operators
ambInfixes :: [String] -> B.Ab a
ambInfixes = Left . B.abortLines "Ambiguous infix operators"

-- | File not found
noFile :: String -> B.Ab a
noFile = Left . B.abortLine "File not found"

-- | Odd relation literal
oddRelation :: B.Ab a
oddRelation = Left $ B.abortBecause "Odd relation literal"

-- | Require flat name
reqFlatName :: B.Token -> B.Ab a
reqFlatName tok = Left $ B.abortLine "Require flat name" n where
    n = B.tokenContent tok

-- | Require term name
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

-- | Unknown term name
unkTerm :: [B.TermName] -> B.Relhead -> B.Ab a
unkTerm ns he1 =
    Left $ B.abortLines "Unknown term name"
         $ detailTermRel "Unknown" ns he1

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

detailTermRel :: [Char] -> [String] -> B.Relhead -> [[Char]]
detailTermRel label ns he1 = detail where
    detail = [label] ++ indent ns' ++ ["Relation"] ++ indent ns1
    indent = map ("  " ++)
    ns'    = map B.showTermName ns
    ns1    = map (show . B.doc) $ B.headTerms he1


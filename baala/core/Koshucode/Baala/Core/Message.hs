{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Message
( -- * Base package
  module Koshucode.Baala.Base.Message,

  -- * Core package
  ambInfixes,
  extraOperand,
  noFile,
  noSlotName,
  noSlotIndex,
  oddRelation,
  reqFlatName,
  reqOperand,
  reqOperandName,
  reqTermName,
  unexpOperand,
  unkClause,
  unkCop,
  unkCox,
  unkNestRel,
  unkRelmap,
  unkTerm,
  unkWithVar,
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

-- | Extra operand
extraOperand :: B.Ab a
extraOperand = Left $ B.abortBecause "Extra operand"

-- | File not found
noFile :: String -> B.Ab a
noFile = Left . B.abortLine "File not found"

-- | No slot content
noSlotName :: Int -> String -> B.Ab a
noSlotName n name = Left $ B.abortLine "No slot content" $ detail n where
    detail 0 = "Positional operand @'" ++ name
    detail 1 = "Named operand -"       ++ name
    detail 2 = "Global slot @@"        ++ name
    detail a = "Unknown slot level "   ++ show a

-- | No slot content
noSlotIndex :: [String] -> Int -> B.Ab a
noSlotIndex xs n = Left $ B.abortLines "No slot content" $
                   ("No index @'" ++ show n ++ " in") : xs

-- | Odd relation literal
oddRelation :: B.Ab a
oddRelation = Left $ B.abortBecause "Odd relation literal"

-- | Require flat name
reqFlatName :: B.Token -> B.Ab a
reqFlatName tok = Left $ B.abortLine "Require flat name" n where
    n = B.tokenContent tok

-- | Require operand
reqOperand :: String -> B.Ab a
reqOperand = Left . B.abortLine "Require operand"

-- | Require operand
reqOperandName :: String -> B.Ab a
reqOperandName = Left . B.abortLine "Require operand name, e.g., -xxx"

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

-- | Unknown content operator
unkCop :: String -> B.Ab a
unkCop = Left . B.abortLine "Unknown content operator"

-- | Unknown nested relation
unkNestRel :: String -> B.Ab a
unkNestRel = Left . B.abortLine "Unknown nested relation"

-- | Unknown relmap operator
unkRelmap :: String -> B.Ab a
unkRelmap = Left . B.abortLine "Unknown relmap operator"

-- | Unknown term name
unkTerm :: [B.TermName] -> B.Relhead -> B.Ab a
unkTerm ns he1 =
    Left $ B.abortLines "Unknown term name"
         $ detailTermRel "Unknown" ns he1

unkWithVar :: String -> B.Ab a
unkWithVar = Left . B.abortLine "Unknown with-variable"

-- | Unknown word
unkWord :: String -> B.Ab a
unkWord = Left . B.abortLine "Unknown word"

unmatchType :: String -> B.Ab a
unmatchType = Left . B.abortLine "Type unmatch"

-- | Unresolved prefix
unresPrefix :: B.Ab a
unresPrefix = Left $ B.abortBecause "Unresolved prefix"

detailTermRel :: String -> [String] -> B.Relhead -> [String]
detailTermRel label ns he1 = detail where
    detail = [label] ++ indent ns' ++ ["Relation"] ++ indent ns1
    indent = map ("  " ++)
    ns'    = map B.showTermName ns
    ns1    = B.headExplainLines he1


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Message
( -- * Base package
  module Koshucode.Baala.Base.Message,

  -- * Core package
  ambInfixes,
  dupAttr,
  dupPrefix,
  dupReplacement,
  emptyLiteral,
  extraAttr,
  invalidPrefix,
  lackArg,
  noFile,
  noSlotName,
  noSlotIndex,
  oddRelation,
  reqFlatName,
  reqAttr,
  reqAttrName,
  reqTermName,
  unexpAttr,
  unkBracket,
  unkClause,
  unkCop,
  unkCox,
  unkGlobalVar,
  unkNestRel,
  unkRefVar,
  unkRelmap,
  unkShow,
  unkTerm,
  unkWithVar,
  unkWord,
  unmatchType,
  unmatchBlank,
  unresPrefix,

  -- * Utility
  detailTermRel,
) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Base.Message


-- | Ambiguous infix operators
ambInfixes :: [String] -> B.Ab a
ambInfixes = Left . B.abortLines "Ambiguous infix operators"

-- | Unexpected attribute / Duplicate
dupAttr :: [String] -> B.Ab a
dupAttr ns = unexpAttr $ "Duplicate" ++ unwords ns

-- | Duplicate prefix
dupPrefix :: [String] -> B.Ab a
dupPrefix = Left . B.abortLine "Duplicate prefix" . unwords

-- | Duplicate replacement
dupReplacement :: [String] -> B.Ab a
dupReplacement = Left . B.abortLine "Duplicate replacement" . unwords

-- | Empty literal
emptyLiteral :: B.Ab a
emptyLiteral = Left $ B.abortBecause "Empty literal"

-- | Extra attribute
extraAttr :: B.Ab a
extraAttr = Left $ B.abortBecause "Extra attribute"

-- | Invalid prefix character
invalidPrefix :: [String] -> B.Ab a
invalidPrefix = Left . B.abortLine "Invalid prefix character" . unwords

-- | File not found
noFile :: String -> B.Ab a
noFile = Left . B.abortLine "File not found"

-- | No slot content
noSlotName :: Int -> String -> B.Ab a
noSlotName n name = Left $ B.abortLine "No slot content" $ detail n where
    detail 0 = "Positional attribute @'" ++ name
    detail 1 = "Named attribute -"       ++ name
    detail 2 = "Global slot @@"          ++ name
    detail a = "Unknown slot level "     ++ show a

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

-- | Require attribute
reqAttr :: String -> B.Ab a
reqAttr = Left . B.abortLine "Require attribute"

-- | Require attribute
reqAttrName :: String -> B.Ab a
reqAttrName = Left . B.abortLine "Require attribute name, e.g., -xxx"

-- | Require term name
reqTermName :: B.Ab a
reqTermName = Left $ B.abortBecause "Require term name"

-- | Lack of argument
lackArg :: String -> B.Ab a
lackArg = Left . B.abortLine "Lack of argument"

-- | Unexpected attribute
unexpAttr :: String -> B.Ab a
unexpAttr = Left . B.abortLine "Unexpected attribute"

-- | Unknown bracket
unkBracket :: B.Ab a
unkBracket = Left $ B.abortBecause "Unknown bracket"

-- | Unknown clause
unkClause :: B.Ab a
unkClause = Left $ B.abortBecause "Unknown clause"

-- | Unknown expression
unkCox :: String -> B.Ab a
unkCox = Left . B.abortLine "Unknown expression"

-- | Unknown content operator
unkCop :: String -> B.Ab a
unkCop = Left . B.abortLine "Unknown content operator"

-- | Unknown global variable
unkGlobalVar :: String -> B.Ab a
unkGlobalVar = Left . B.abortLine "Unknown global variable"

-- | Unknown nested relation
unkNestRel :: String -> B.Ab a
unkNestRel = Left . B.abortLine "Unknown nested relation"

-- | Unknown reference for variable
unkRefVar :: String -> Int -> B.Ab a
unkRefVar v k = Left $ B.abortLine "Unknown reference for variable"
                     $ var (v, k)

-- | Unknown relmap operator
unkRelmap :: String -> B.Ab a
unkRelmap = Left . B.abortLine "Unknown relmap operator"

-- | Unknown object
unkShow :: (Show x) => x -> B.Ab a
unkShow x = Left $ B.abortLines "Unknown object" $ lines $ show x

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

-- | Unmatch blank (bug)
unmatchBlank :: String -> Int -> String -> [String] -> B.Ab a
unmatchBlank v k _ vs =
    Left $ B.abortLines "Unmatch blank (bug)"
           [ "look up " ++ var (v, k)
           , "in " ++ (unwords $ map var $ zip vs [1..]) ]

var :: (String, Int) -> String
var (v, k) = v ++ "/" ++ show k

-- | Unresolved prefix
unresPrefix :: B.Ab a
unresPrefix = Left $ B.abortBecause "Unresolved prefix"

detailTermRel :: String -> [String] -> B.Relhead -> [String]
detailTermRel label ns he1 = detail where
    detail = [label] ++ indent ns' ++ ["Relation"] ++ indent ns1
    indent = map ("  " ++)
    ns'    = map B.showTermName ns
    ns1    = B.headExplainLines he1


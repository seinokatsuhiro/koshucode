{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Message
( -- * Core package
  module Koshucode.Baala.Core.Message,

  -- * Op package
  checkTerm,
  diffHead,
  dupTerm,
  dumpCox,
  dumpRel,
  noAttr,
  notNestRel,
  oddAttr,
  reqBool,
  reqRel,
  reqCollection,
  reqNewTerm,
  reqUnaryFn,
  unexpTermName,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Core.Message

-- | check-term failed
checkTerm :: String -> [B.TermName] -> B.Relhead -> B.Ab a
checkTerm label ns he =
    Left $ B.abortLines "check-term failed"
         $ detailTermRel label ns he

-- | Different headings
diffHead :: [B.Relhead] -> B.Ab a
diffHead = Left . B.abortLines "Different headings" . map showHead

-- | Dump content expression
dumpCox :: (Show c) => c -> B.Ab a
dumpCox cox = Left $ B.abortLines "Dump content expression"
                   $ lines $ show cox

-- | Dump relation
dumpRel :: (B.Write c, C.CRel c) => B.Rel c -> B.Ab a
dumpRel r = Left $ B.abortPage "Dump relation" $ C.relTableLines [] r

-- | Duplicate term name
dupTerm :: [B.TermName] -> B.Relhead -> B.Ab a
dupTerm ns he =
    Left $ B.abortLines "Duplicate term name"
         $ detailTermRel "Duplicated" ns he

-- | Odd attribute
oddAttr :: B.Ab a
oddAttr = Left $ B.abortBecause "Odd attribute"

-- | Attribute not found
noAttr :: B.Ab a
noAttr = Left $ B.abortBecause "Attribute not found"

-- | Not a nested relation
notNestRel :: [B.TermName] -> B.Relhead -> B.Ab a
notNestRel ns he =
    Left $ B.abortLines "Not a nested relation"
         $ detailTermRel "Given" ns he

-- | Require Boolean
reqBool :: B.Ab a
reqBool = Left $ B.abortBecause "Require Boolean"

-- | Require relation
reqRel :: B.Ab a
reqRel = Left $ B.abortBecause "Require relation"

-- | Require collection type
reqCollection :: B.Ab a
reqCollection = Left $ B.abortBecause "Require collection type"

-- | Require new term names
reqNewTerm :: [B.TermName] -> B.Relhead -> B.Ab a
reqNewTerm ns he =
    Left $ B.abortLines "Require new term names"
         $ detailTermRel "Known" ns he

-- | Require unary function
reqUnaryFn :: B.Ab a
reqUnaryFn = Left $ B.abortBecause "Require unary function"

-- | Unexpected term names
unexpTermName :: B.Ab a
unexpTermName = Left $ B.abortBecause "Unexpected term names"

showHead :: B.Relhead -> String
showHead = unwords . B.headNames

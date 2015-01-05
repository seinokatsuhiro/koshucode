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
    dumpTrees,
    noAttr,
    notImpl,
    notNestRel,
    oddAttr,
    reqBool,
    reqRel,
    reqCollection,
    reqInterp,
    reqNewTerm,
    reqUnaryFn,
    unexpTermName,
  ) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Core.Message


-- ----------------------  Op package

-- | check-term failed
checkTerm :: String -> [B.TermName] -> B.Head -> B.Ab a
checkTerm label ns he =
    Left $ B.abortLines "check-term failed"
         $ detailTermRel label ns he

-- | Different headings
diffHead :: [B.Head] -> B.Ab a
diffHead = Left . B.abortLines "Different headings" . map showHead

-- | Dump content expression
dumpCox :: (Show c) => c -> B.Ab a
dumpCox cox = Left $ B.abortLines "Dump content expression"
                   $ lines $ show cox

-- | Dump relation
dumpRel :: (B.Write c, C.CRel c) => B.Rel c -> B.Ab a
dumpRel r = Left $ B.abortPage "Dump relation" $ C.relTableLines [] r

-- | Dump token trees
dumpTrees :: [B.TTree] -> B.Ab a
dumpTrees trees = Left $ B.abortLines "Dump token trees"
                   $ lines $ show $ B.ttDoc trees

-- | Duplicate term name
dupTerm :: [B.TermName] -> B.Head -> B.Ab a
dupTerm ns he =
    Left $ B.abortLines "Duplicate term name"
         $ detailTermRel "Duplicated" ns he

-- | Odd attribute
oddAttr :: B.Ab a
oddAttr = Left $ B.abortBecause "Odd attribute"

-- | Attribute not found
noAttr :: String -> B.Ab a
noAttr n = Left $ B.abortLine "Attribute not found" n

-- | Not implemented
notImpl :: B.Ab a
notImpl = Left $ B.abortBecause "Not implemented"

-- | Not a nested relation
notNestRel :: [B.TermName] -> B.Head -> B.Ab a
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

-- | Require data interpretation
reqInterp :: B.Ab a
reqInterp = Left $ B.abortBecause "Require data interpretation"

-- | Require new term names
reqNewTerm :: [B.TermName] -> B.Head -> B.Ab a
reqNewTerm ns he =
    Left $ B.abortLines "Require new term names"
         $ detailTermRel "Known" ns he

-- | Require unary function
reqUnaryFn :: B.Ab a
reqUnaryFn = Left $ B.abortBecause "Require unary function"

-- | Unexpected term names
unexpTermName :: B.Ab a
unexpTermName = Left $ B.abortBecause "Unexpected term names"

showHead :: B.Head -> String
showHead = unwords . B.headNames

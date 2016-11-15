{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Rop.Flat.Message
  ( module Koshucode.Baala.Rop.Base.Message,
    checkTerm,
    diffHead,
    dupTerm,
    dumpCox,
    dumpRel,
    dumpTrees,
    oddAttr,
    reqBool,
    reqRel,
    reqCollection,
    reqInterp,
    reqNewTerm,
    reqUnaryFn,
    unexpTermName,
    unmatchShare,
  ) where

import qualified Koshucode.Baala.Base           as B
import qualified Koshucode.Baala.Syntax         as S
import qualified Koshucode.Baala.Data           as D
import qualified Koshucode.Baala.Core           as C
import qualified Koshucode.Baala.Data.Message   as Msg
import Koshucode.Baala.Rop.Base.Message


-- ----------------------  Op package

-- | check-term failed
checkTerm :: String -> [S.TermName] -> D.Head -> B.Ab a
checkTerm label ns he =
    Left $ B.abortLines "check-term failed"
         $ detailTermRel label ns he

-- | Different headings
diffHead :: [D.Head] -> B.Ab a
diffHead = Left . B.abortLines "Different headings" . map showHead

-- | Dump content expression
dumpCox :: (Show c) => c -> B.Ab a
dumpCox cox = Left $ B.abortLines "Dump content expression"
                   $ lines $ show cox

-- | Dump relation
dumpRel :: (D.CRel c, B.MixShortEncode c) => D.Rel c -> B.Ab a
dumpRel r = Left $ B.abortPage "Dump relation" $ C.relTableLines [] r

-- | Dump token trees
dumpTrees :: [S.TTree] -> B.Ab a
dumpTrees trees = Left $ B.abortLines "Dump token trees"
                   $ lines $ show $ S.ttDoc trees

-- | Duplicate term name
dupTerm :: (D.GetTermNames t) => t -> B.Ab a
dupTerm t =
    Left $ B.abortLines "Duplicate term name"
         $ msgTerms2 "Duplicate" t' "in the terms" t
        where t' = B.duplicates $ D.getTermNames t

-- | Odd attribute
oddAttr :: B.Ab a
oddAttr = Left $ B.abortBecause "Odd attribute"

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
reqNewTerm :: (D.GetTermNames t1, D.GetTermNames t2) => t1 -> t2 -> B.Ab a
reqNewTerm t1 t2 =
    Left $ B.abortLines "Require new term names"
         $ Msg.msgTerms2 "Present" t1 "in the terms" t2

-- | Require unary function
reqUnaryFn :: B.Ab a
reqUnaryFn = Left $ B.abortBecause "Require unary function"

-- | Unexpected term names
unexpTermName :: B.Ab a
unexpTermName = Left $ B.abortBecause "Unexpected term names"

showHead :: D.Head -> String
showHead = unwords . D.getTermNames

-- | Unmatch shared terms.
unmatchShare :: [S.TermName] -> [S.TermName] -> B.Ab a
unmatchShare e a =
    Left $ B.abortLines "Unmatch shared terms"
         $ expectActual (ts e) (ts a)
    where ts xs = unwords $ S.termNameString <$> xs


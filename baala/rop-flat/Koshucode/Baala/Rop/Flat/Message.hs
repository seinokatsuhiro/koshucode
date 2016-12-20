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
    B.leftLines "check-term failed"
         $ detailTermRel label ns he

-- | Different headings
diffHead :: [D.Head] -> B.Ab a
diffHead = B.leftLines "Different headings" . map showHead

-- | Dump content expression
dumpCox :: (Show c) => c -> B.Ab a
dumpCox cox = B.leftLines "Dump content expression"
                   $ lines $ show cox

-- | Dump relation
dumpRel :: (D.CRel c, B.MixEncode c) => D.Rel c -> B.Ab a
dumpRel r = B.leftPage "Dump relation" $ C.relTableLines [] r

-- | Dump token trees
dumpTrees :: [S.Tree] -> B.Ab a
dumpTrees trees = B.leftLines "Dump token trees"
                   $ lines $ show $ S.treesDoc trees

-- | Duplicate term name
dupTerm :: (D.GetTermNames t) => t -> B.Ab a
dupTerm t =
    B.leftLines "Duplicate term name"
         $ msgTerms2 "Duplicate" t' "in the terms" t
        where t' = B.duplicates $ D.getTermNames t

-- | Odd attribute
oddAttr :: B.Ab a
oddAttr = B.leftBecause "Odd attribute"

-- | Require Boolean
reqBool :: B.Ab a
reqBool = B.leftBecause "Require Boolean"

-- | Require relation
reqRel :: B.Ab a
reqRel = B.leftBecause "Require relation"

-- | Require collection type
reqCollection :: B.Ab a
reqCollection = B.leftBecause "Require collection type"

-- | Require data interpretation
reqInterp :: B.Ab a
reqInterp = B.leftBecause "Require data interpretation"

-- | Require new term names
reqNewTerm :: (D.GetTermNames t1, D.GetTermNames t2) => t1 -> t2 -> B.Ab a
reqNewTerm t1 t2 =
    B.leftLines "Require new term names"
         $ Msg.msgTerms2 "Present" t1 "in the terms" t2

-- | Require unary function
reqUnaryFn :: B.Ab a
reqUnaryFn = B.leftBecause "Require unary function"

-- | Unexpected term names
unexpTermName :: B.Ab a
unexpTermName = B.leftBecause "Unexpected term names"

showHead :: D.Head -> String
showHead = unwords . map S.termNameString . D.getTermNames

-- | Unmatch shared terms.
unmatchShare :: [S.TermName] -> [S.TermName] -> B.Ab a
unmatchShare e a =
    B.leftLines "Unmatch shared terms"
         $ expectActual (ts e) (ts a)
    where ts xs = unwords $ S.termNameString <$> xs


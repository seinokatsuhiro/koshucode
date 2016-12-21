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

import qualified Koshucode.Baala.DataPlus       as K
import qualified Koshucode.Baala.Core           as C
import qualified Koshucode.Baala.Data.Message   as Msg
import Koshucode.Baala.Rop.Base.Message


-- ----------------------  Op package

-- | check-term failed
checkTerm :: String -> [K.TermName] -> K.Head -> K.Ab a
checkTerm label ns he =
    K.leftLines "check-term failed"
         $ detailTermRel label ns he

-- | Different headings
diffHead :: [K.Head] -> K.Ab a
diffHead = K.leftLines "Different headings" . map showHead

-- | Dump content expression
dumpCox :: (Show c) => c -> K.Ab a
dumpCox cox = K.leftLines "Dump content expression"
                   $ lines $ show cox

-- | Dump relation
dumpRel :: (K.CRel c, K.MixEncode c) => K.Rel c -> K.Ab a
dumpRel r = K.leftPage "Dump relation" $ C.relTableLines [] r

-- | Dump token trees
dumpTrees :: [K.Tree] -> K.Ab a
dumpTrees trees = K.leftLines "Dump token trees"
                   $ lines $ show $ K.treesDoc trees

-- | Duplicate term name
dupTerm :: (K.GetTermNames t) => t -> K.Ab a
dupTerm t =
    K.leftLines "Duplicate term name"
         $ msgTerms2 "Duplicate" t' "in the terms" t
        where t' = K.duplicates $ K.getTermNames t

-- | Odd attribute
oddAttr :: K.Ab a
oddAttr = K.leftBecause "Odd attribute"

-- | Require Boolean
reqBool :: K.Ab a
reqBool = K.leftBecause "Require Boolean"

-- | Require relation
reqRel :: K.Ab a
reqRel = K.leftBecause "Require relation"

-- | Require collection type
reqCollection :: K.Ab a
reqCollection = K.leftBecause "Require collection type"

-- | Require data interpretation
reqInterp :: K.Ab a
reqInterp = K.leftBecause "Require data interpretation"

-- | Require new term names
reqNewTerm :: (K.GetTermNames t1, K.GetTermNames t2) => t1 -> t2 -> K.Ab a
reqNewTerm t1 t2 =
    K.leftLines "Require new term names"
         $ Msg.msgTerms2 "Present" t1 "in the terms" t2

-- | Require unary function
reqUnaryFn :: K.Ab a
reqUnaryFn = K.leftBecause "Require unary function"

-- | Unexpected term names
unexpTermName :: K.Ab a
unexpTermName = K.leftBecause "Unexpected term names"

showHead :: K.Head -> String
showHead = unwords . map K.termNameString . K.getTermNames

-- | Unmatch shared terms.
unmatchShare :: [K.TermName] -> [K.TermName] -> K.Ab a
unmatchShare e a =
    K.leftLines "Unmatch shared terms"
         $ expectActual (ts e) (ts a)
    where ts xs = unwords $ K.termNameString <$> xs


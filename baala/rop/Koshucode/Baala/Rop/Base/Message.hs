{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Rop.Base.Message
  ( module Koshucode.Baala.Core.Message,
    abPara,

    -- * Messages
    -- ** Terms
    checkTerm,
    dupTerm,
    reqNewTerm,
    unevenTerms,
    unmatchShare,

    -- ** Require
    reqBool,
    reqRawText,
    reqSingleEqual,
    reqCollection,
    reqInterp,
    reqRel,
    reqRelmap,
    reqUnaryFn,

    -- ** Dump
    dumpCox,
    dumpRel,
    dumpTrees,

    -- ** Others
    noAttr,
    notImpl,
    diffHead,
    notNestRel,
    unkTag,
  ) where

import Koshucode.Baala.Core.Message
import qualified Koshucode.Baala.DataPlus  as K
import qualified Koshucode.Baala.Core      as C


-- | Abortable scope for relmap parameter.
abPara :: (K.GetCodePos cp) => K.Abortable cp b
abPara = K.abortable "parameter"


-- ============================================  Messages

-- ---------------------------------  Terms

-- | [check-term failed]
checkTerm :: String -> [K.TermName] -> K.Head -> K.Ab a
checkTerm label ns he =
    K.leftLines "check-term failed"
         $ detailTermRel label ns he

-- | [Duplicate term names] Duplicate ... in the terms ...
dupTerm :: (K.GetTermNames t) => t -> K.Ab a
dupTerm t =
    K.leftLines "Duplicate term names"
         $ msgTerms2 "Duplicate" t' "in the terms" t
        where t' = K.duplicates $ K.getTermNames t

-- | [Require new term names] Present ... in the terms ...
reqNewTerm :: (K.GetTermNames t) => K.TermPicker c -> t -> K.Ab a
reqNewTerm pk input =
    K.leftLines "Require new term names"
         $ msgTerms2 "Present" (K.preTerms pk) "in the terms" input

-- | [Uneven terms] /N/ and /N/ terms
unevenTerms :: (K.GetTermNames t1, K.GetTermNames t2) => t1 -> t2 -> K.Ab a
unevenTerms t1 t2 = K.leftLine "Uneven terms" detail where
    detail = n1 ++ " and " ++ n2 ++ " terms"
    n1 = show . length $ K.getTermNames t1
    n2 = show . length $ K.getTermNames t2

-- | [Unmatch shared terms]
unmatchShare :: [K.TermName] -> [K.TermName] -> K.Ab a
unmatchShare e a =
    K.leftLines "Unmatch shared terms"
         $ expectActual (ts e) (ts a)
    where ts xs = unwords $ K.termNameString <$> xs

-- ---------------------------------  Require

-- | [Require Boolean]
reqBool :: K.Ab a
reqBool = K.leftBecause "Require Boolean"

-- | [Require unquoted text]
reqRawText :: K.Ab a
reqRawText = K.leftBecause "Require unquoted text"

-- | [Require single equal sign]
reqSingleEqual :: K.Ab a
reqSingleEqual = K.leftBecause "Require single equal sign"

-- | [Require collection]
reqCollection :: K.Ab a
reqCollection = K.leftBecause "Require collection"

-- | [Require data interpretation]
reqInterp :: K.Ab a
reqInterp = K.leftBecause "Require data interpretation"

-- | [Require relation]
reqRel :: K.Ab a
reqRel = K.leftBecause "Require relation"

-- | [Require no relmaps]
--   [Require one relmap]
--   [Require /N/ relmaps]
reqRelmap :: Int -> K.Ab a
reqRelmap 0 = K.leftBecause "Require no relmaps"
reqRelmap 1 = K.leftBecause "Require one relmap"
reqRelmap n = K.leftBecause $ "Require " ++ show n ++ " relmaps"

-- | [Require unary function]
reqUnaryFn :: K.Ab a
reqUnaryFn = K.leftBecause "Require unary function"

-- ---------------------------------  Dump

-- | [Dump content expression]
dumpCox :: (Show c) => c -> K.Ab a
dumpCox cox = K.leftLines "Dump content expression"
                   $ lines $ show cox

-- | [Dump relation]
dumpRel :: (K.CRel c, K.MixEncode c) => K.Rel c -> K.Ab a
dumpRel r = K.leftPage "Dump relation" $ C.relTableLines [] r

-- | [Dump token trees]
dumpTrees :: [K.Tree] -> K.Ab a
dumpTrees = K.leftLines "Dump token trees" . K.ppRawTrees

-- ---------------------------------  Others

-- | [Attribute not found]
noAttr :: String -> K.Ab a
noAttr n = K.leftLine "Attribute not found" n

-- | [Not implemented]
notImpl :: K.Ab a
notImpl = K.leftBecause "Not implemented"

-- | [Different headings] \/T ..., \/T ...
diffHead :: [K.Head] -> K.Ab a
diffHead = K.leftLines "Different headings" . map showHead

showHead :: K.Head -> String
showHead = unwords . map K.termNameString . K.getTermNames

-- | [Not a nested relation]
notNestRel :: (K.GetTermNames t) => t -> K.Head -> K.Ab a
notNestRel t he =
    K.leftLines "Not a nested relation"
         $ detailTermRel "Given" (K.getTermNames t) he

-- | [BUG: Unknown tag]
unkTag :: K.Ab a
unkTag = K.leftBecause "BUG: Unknown tag"

{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Rop.Base.Message
  ( module Koshucode.Baala.Core.Message,

    -- * Messages
    -- ** Terms
    dupTerm,
    reqNewTerm,
    unevenTerms,
    unmatchShare,

    -- ** Dump
    dumpCox,
    dumpRel,
    dumpTrees,

    -- ** Others
    noAttr,
    notImpl,
    diffHead,
    reqBool,
    reqInterp,
    reqRel,
    reqRelmap,
    reqUnaryFn,
    notNestRel,
  ) where

import Koshucode.Baala.Core.Message
import qualified Koshucode.Baala.DataPlus  as K
import qualified Koshucode.Baala.Core      as C


-- ============================================  Messages

-- ---------------------------------  Terms

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
dumpTrees trees = K.leftLines "Dump token trees"
                   $ lines $ show $ K.treesDoc trees

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

-- | [Require Boolean]
reqBool :: K.Ab a
reqBool = K.leftBecause "Require Boolean"

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

-- | [Not a nested relation]
notNestRel :: (K.GetTermNames t) => t -> K.Head -> K.Ab a
notNestRel t he =
    K.leftLines "Not a nested relation"
         $ detailTermRel "Given" (K.getTermNames t) he


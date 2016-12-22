{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Rop.Base.Message
  ( module Koshucode.Baala.Core.Message,

    -- * Messages
    -- ** Terms
    dupTerm,
    reqNewTerm,
    unevenTerms,

    -- ** Others
    noAttr,
    notImpl,
    reqRelmap,
    notNestRel,
  ) where

import Koshucode.Baala.Core.Message
import qualified Koshucode.Baala.DataPlus as K


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

-- ---------------------------------  Others

-- | [Attribute not found]
noAttr :: String -> K.Ab a
noAttr n = K.leftLine "Attribute not found" n

-- | [Not implemented]
notImpl :: K.Ab a
notImpl = K.leftBecause "Not implemented"

-- | [Require no relmaps]
--   [Require one relmap]
--   [Require /N/ relmaps]
reqRelmap :: Int -> K.Ab a
reqRelmap 0 = K.leftBecause "Require no relmaps"
reqRelmap 1 = K.leftBecause "Require one relmap"
reqRelmap n = K.leftBecause $ "Require " ++ show n ++ " relmaps"

-- | [Not a nested relation]
notNestRel :: (K.GetTermNames t) => t -> K.Head -> K.Ab a
notNestRel t he =
    K.leftLines "Not a nested relation"
         $ detailTermRel "Given" (K.getTermNames t) he


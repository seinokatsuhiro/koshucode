{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Rop.Base.Message
  ( module Koshucode.Baala.Core.Message,

    -- * Messages
    -- ** Terms
    dupTerm,
    unevenTerms,

    -- ** Others
    noAttr,
    notImpl,
    reqRelmap,
  ) where

import Koshucode.Baala.Core.Message
import qualified Koshucode.Baala.DataPlus as K

-- ============================================  Messages

-- ---------------------------------  Terms

-- | [Duplicate term name] Duplicate ... in the terms ...
dupTerm :: (K.GetTermNames t) => t -> K.Ab a
dupTerm t =
    K.leftLines "Duplicate term name"
         $ msgTerms2 "Duplicate" t' "in the terms" t
        where t' = K.duplicates $ K.getTermNames t

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


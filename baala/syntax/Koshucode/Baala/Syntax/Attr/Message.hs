{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Syntax.Attr.Message
  ( -- * Abortable
    abAttr,
    abAttrTrees,
    abSlot,
    -- * Message
    dupAttr,
    extraAttr,
    noSlotName,
    noSlotIndex,
    reqAttr,
    reqAttrName,
    unexpAttr,
    unexpAttrMulti,
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Para             as S
import qualified Koshucode.Baala.Syntax.Symbol           as S

-- | Abortable scope for attribute.
abAttr :: (B.GetCodePos cp) => B.Abortable cp b
abAttr = B.abortable "attr"

-- | Abortable scope for attribute.
abAttrTrees :: (B.GetCodePos cp) => B.Abortable cp b
abAttrTrees = B.abortable "attr"

-- | Abortable scope for slot.
abSlot :: (B.GetCodePos cp) => B.Abortable cp b
abSlot = B.abortable "slot"

-- | Unexpected attribute / Duplicate
dupAttr :: [S.TermName] -> B.Ab a
dupAttr ns = unexpAttr $ "Duplicate " ++ unwords (map S.termNameString ns)

-- | Extra attribute
extraAttr :: B.Ab a
extraAttr = Left $ B.abortBecause "Extra attribute"

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

-- | Require attribute
reqAttr :: String -> B.Ab a
reqAttr = Left . B.abortLine "Require attribute"

-- | Require attribute
reqAttrName :: String -> B.Ab a
reqAttrName = Left . B.abortLine "Require attribute name, e.g., -xxx"

-- | Unexpected attribute
unexpAttr :: String -> B.Ab a
unexpAttr = Left . B.abortLine "Unexpected attribute"

-- | Unmatch any patterns
unexpAttrMulti :: [S.ParaUnmatch String] -> B.Ab a
unexpAttrMulti [u] = Left $ B.abortLines "Unexpected attribute" [attrDetail u]
unexpAttrMulti us  = Left $ B.abortLines "Unmatch any patterns" $ map attrDetail us

attrDetail :: S.ParaUnmatch String -> String
attrDetail u =
    case u of
      S.ParaOutOfRange _ p  -> require $ S.paraMinLength p
      S.ParaUnknown  ns     -> "Unknown "  ++ unwords ns
      S.ParaMissing  ns     -> "Missing "  ++ unwords ns
      S.ParaMultiple ns     -> "Repeated " ++ unwords ns
    where
      require 0 = "Attributes not required"
      require 1 = "Require one attribute"
      require 2 = "Require two attributes"
      require 3 = "Require three attributes"
      require n = "Require " ++ show n ++ " attributes"


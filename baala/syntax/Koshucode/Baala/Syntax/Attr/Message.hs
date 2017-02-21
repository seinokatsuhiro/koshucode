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

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Para             as S
import qualified Koshucode.Baala.Syntax.Symbol           as S
import qualified Koshucode.Baala.Syntax.Token            as S

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
extraAttr = B.leftBecause "Extra attribute"

-- | No slot content
noSlotName :: (O.Textual t) => S.SlotType -> t -> B.Ab a
noSlotName t name = B.leftLine "No slot content" $ detail t where
    detail S.SlotPos    = "Positional parameter @'" O.++ O.tString name
    detail S.SlotNamed  = "Named parameter -"       O.++ O.tString name
    detail S.SlotGlobal = "Global slot @@"          O.++ O.tString name

-- | No slot content
noSlotIndex :: [String] -> Int -> B.Ab a
noSlotIndex xs n = B.leftLines "No slot content" $
                   ("No index @'" ++ show n ++ " in") : xs

-- | Require attribute
reqAttr :: (O.Textual t) => t -> B.Ab a
reqAttr = B.leftLine "Require attribute"

-- | Require attribute
reqAttrName :: (O.Textual t) => t -> B.Ab a
reqAttrName = B.leftLine "Require attribute name, e.g., -xxx"

-- | Unexpected attribute
unexpAttr :: String -> B.Ab a
unexpAttr = B.leftLine "Unexpected attribute"

-- | Unmatch any patterns
unexpAttrMulti :: [(String, S.ParaUnmatch String)] -> B.Ab a
unexpAttrMulti [u] = B.leftLines "Unexpected attribute" $ attrDetail u
unexpAttrMulti us  = B.leftLines "Unmatch any patterns" $ concatMap attrDetail us

attrDetail :: (O.Textual t) => (String, S.ParaUnmatch t) -> [String]
attrDetail (usage, unmatch) =
    case unmatch of
      S.ParaPos n _      -> expect ["Unmatch " ++ show n ++ " positional attributes"]
      S.ParaUnknown  ns  -> expect ["Unknown "  ++ unwords (O.tString <$> ns)]
      S.ParaMissing  ns  -> expect ["Missing "  ++ unwords (O.tString <$> ns)]
      S.ParaMultiple ns  -> expect ["Repeated " ++ unwords (O.tString <$> ns)]
    where
      expect reason = reason ++ ["  against '" ++ usage ++ "'"]

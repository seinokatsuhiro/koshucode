{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Syntax.Attr.Message
  ( -- * Abortable
    abAttr,
    abAttrTrees,
    abSlot,
    abSlotTree,
    abortableTree,
    abortableTrees,
    -- * Message
    dupAttr,
    extraAttr,
    noSlotName,
    noSlotIndex,
    reqAttr,
    reqAttrName,
    unexpAttr,
    unexpAttr0,
    unexpAttr1,
    unexpAttr2,
    unexpAttr3,
    unexpAttr4,
    unexpAttr1V,
    unexpAttr1Q,
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Symbol           as D
import qualified Koshucode.Baala.Syntax.Token            as D

abAttr        :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abAttr        = B.abortable "attr"

abAttrTrees   :: D.TTreesTo (B.Map (B.Ab b))
abAttrTrees   = abortableTrees "attr"

abSlot        :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abSlot        = B.abortable "slot"

abSlotTree    :: D.TTreeTo (B.Map (B.Ab b))
abSlotTree    = abortableTree "slot"

-- | Same as 'abortable' except for using 'D.TTree'
--   instead of list of 'D.Token'.
abortableTree :: String -> D.TTreeTo (B.Map (B.Ab b))
abortableTree tag = B.abortable tag . B.untree

-- | Same as 'abortable' except for using list of 'D.TTree'
--   instead of list of 'D.Token'.
abortableTrees :: String -> D.TTreesTo (B.Map (B.Ab b))
abortableTrees tag = B.abortable tag . B.untrees

-- | Unexpected attribute / Duplicate
dupAttr :: [String] -> B.Ab a
dupAttr ns = unexpAttr $ "Duplicate " ++ unwords (map D.showTermName ns)

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

unexpAttr0 :: B.Ab a
unexpAttr0 = unexpAttr "Attributes not required"

unexpAttr1 :: B.Ab a
unexpAttr1 = unexpAttr "Require one attribute"

unexpAttr2 :: B.Ab a
unexpAttr2 = unexpAttr "Require two attributes"

unexpAttr3 :: B.Ab a
unexpAttr3 = unexpAttr "Require three attributes"

unexpAttr4 :: B.Ab a
unexpAttr4 = unexpAttr "Require four attributes"

unexpAttr1V :: B.Ab a
unexpAttr1V = unexpAttr "Require attributes"

unexpAttr1Q :: B.Ab a
unexpAttr1Q = unexpAttr "Require one or two attributes"


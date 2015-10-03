{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Lexmap.Message
  ( -- * Abortable
    abAttr,
    abAttrTrees,
    abLexmap,
    abSlot,
    abSlotTree,
    -- * Message
    ambRelmap,
    dupAttr,
    extraAttr,
    noSlotName,
    noSlotIndex,
    reqAttr,
    reqAttrName,
    reqGroup,
    unexpAttr,
    unexpAttr0,
    unexpAttr1,
    unexpAttr2,
    unexpAttr3,
    unexpAttr4,
    unexpAttr1V,
    unexpAttr1Q,
    unkRelmap,
  ) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Data          as D
import qualified Koshucode.Baala.Data.Message  as Msg

abAttr        :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abAttr        = B.abortable "attr"

abAttrTrees   :: D.TTreesTo (B.Map (B.Ab b))
abAttrTrees   = Msg.abortableTrees "attr"

abLexmap      :: D.TTreesTo (B.Map (B.Ab b))
abLexmap      = Msg.abortableTrees "lexmap"

abSlot        :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abSlot        = B.abortable "slot"

abSlotTree    :: D.TTreeTo (B.Map (B.Ab b))
abSlotTree    = Msg.abortableTree "slot"

-- | Ambiguous relmaps
ambRelmap :: String -> [d] -> B.Ab a
ambRelmap name ds = Left $ B.abortLine "Ambiguous relmaps"
                         $ name ++ " (" ++ show (length ds) ++ ")"

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

-- | Require grouping paren
reqGroup :: B.Ab a
reqGroup = Left $ B.abortBecause "Require grouping parens"

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

-- | Unknown relmap operator
unkRelmap :: String -> B.Ab a
unkRelmap = Left . B.abortLine "Unknown relmap operator"


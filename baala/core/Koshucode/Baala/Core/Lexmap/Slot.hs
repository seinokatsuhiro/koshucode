{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Slot substitution.

module Koshucode.Baala.Core.Lexmap.Slot
  ( GlobalSlot,
    substSlot,
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Data                   as D
import qualified Koshucode.Baala.Core.Lexmap.AttrPos    as C
import qualified Koshucode.Baala.Core.Message           as Msg

type GlobalSlot = D.NamedTrees

-- | Substitute slots by global and attribute slots.
substSlot :: [GlobalSlot] -> [C.AttrTree] -> B.AbMap [D.TTree]
substSlot gslot attr = Right . concat B.<=< mapM (substTree gslot attr)

substTree :: [GlobalSlot] -> [C.AttrTree] -> D.TTree -> B.Ab [D.TTree]
substTree gslot attr tree = Msg.abSlotTree tree $ loop tree where
    loop (B.TreeB p q sub) = do sub' <- mapM loop sub
                                Right [B.TreeB p q $ concat sub']
    loop (B.TreeL (D.TSlot _ n name))
        | n == 0    = replace n name C.attrNameTrunk attr (`pos` name)
        | n == 1    = replace n name (C.AttrNormal name) attr Right
        | n == 2    = replace n name name gslot Right
        | otherwise = Msg.noSlotName n name
    loop tk = Right [tk]

    replace n name key assoc f =
        case lookup key assoc of
          Just od -> f od
          Nothing -> Msg.noSlotName n name

    pos :: [D.TTree] -> String -> B.Ab [D.TTree]
    pos od "all" = Right od
    pos od n     = case B.readInt n of
                     Just i  -> Right . B.li1 =<< od `at` i
                     Nothing -> Msg.noSlotName 0 n

    at = substIndex $ unwords . map D.tokenContent . B.untree

substIndex :: (a -> String) -> [a] -> Int -> B.Ab a
substIndex toString xxs n = loop xxs n where
    loop (x : _)  1 = Right x
    loop (_ : xs) i = loop xs $ i - 1
    loop _ _        = Msg.noSlotIndex (number $ map toString xxs) n
    number xs = map pair $ zip [1 :: Int ..] xs
    pair (i, x) = "@'" ++ show i ++ " = " ++ x


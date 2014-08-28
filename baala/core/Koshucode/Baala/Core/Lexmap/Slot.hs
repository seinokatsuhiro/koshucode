{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Slot substitution.

module Koshucode.Baala.Core.Lexmap.Slot
( GlobalSlot,
  substSlot,
) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Lexmap.Attribute  as C
import qualified Koshucode.Baala.Core.Message           as Message

type GlobalSlot = B.NamedTrees

-- | Substitute slots by global and attribute slots.
substSlot :: [GlobalSlot] -> C.AttrTrees -> B.AbMap [B.TokenTree]
substSlot gslot roa = Right . concat B.<=< mapM (substTree gslot roa)

substTree :: [GlobalSlot] -> C.AttrTrees -> B.TokenTree -> B.Ab [B.TokenTree]
substTree gslot roa tree = Message.abSlotTree tree $ loop tree where
    loop (B.TreeB p q sub) = do sub' <- mapM loop sub
                                Right [B.TreeB p q $ concat sub']
    loop (B.TreeL (B.TSlot _ n name))
        | n == 0    = replace n name C.attrNameTrunk roa (`pos` name)
        | n == 1    = replace n name (C.AttrTree $ '-' : name) roa   Right
        | n == 2    = replace n name name            gslot Right
        | otherwise = Message.noSlotName n name
    loop tk = Right [tk]

    replace n name key assoc f =
        case lookup key assoc of
          Just od -> f od
          Nothing -> Message.noSlotName n name

    pos :: [B.TokenTree] -> String -> B.Ab [B.TokenTree]
    pos od "all" = Right od
    pos od n     = case B.readInt n of
                     Just i  -> Right . B.li1 =<< od `at` i
                     Nothing -> Message.noSlotName 0 n

    at = substIndex $ unwords . map B.tokenContent . B.untree

substIndex :: (a -> String) -> [a] -> Int -> B.Ab a
substIndex toString xxs n = loop xxs n where
    loop (x : _)  1 = Right x
    loop (_ : xs) i = loop xs $ i - 1
    loop _ _        = Message.noSlotIndex (number $ map toString xxs) n
    number xs = map pair $ zip [1 :: Int ..] xs
    pair (i, x) = "@'" ++ show i ++ " = " ++ x


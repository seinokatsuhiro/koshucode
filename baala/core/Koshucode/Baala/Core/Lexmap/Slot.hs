{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Slot substitution.

module Koshucode.Baala.Core.Lexmap.Slot
( GlobalSlot,
  slotTrees,
) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Lexmap.Operand  as C
import qualified Koshucode.Baala.Core.Message         as Message

type GlobalSlot = B.NamedTrees

slotTrees :: [GlobalSlot] -> C.Rod -> B.AbMap [B.TokenTree]
slotTrees gslot rod trees =
    do trees' <- slotTree gslot rod `mapM` trees
       Right $ concat trees'

slotTree :: [GlobalSlot] -> C.Rod -> B.TokenTree -> B.Ab [B.TokenTree]
slotTree gslot rod tree = B.abortableTree "slot" tree $ loop tree where
    loop (B.TreeB p q sub) = do sub' <- mapM loop sub
                                Right [B.TreeB p q $ concat sub']
    loop (B.TreeL (B.TSlot _ n name))
        | n == 0    = replace n name "@trunk"     rod  (`pos` name)
        | n == 1    = replace n name ('-' : name) rod  Right
        | n == 2    = replace n name name         gslot Right
        | otherwise = Message.noSlotName n name
    loop tk = Right [tk]

    replace n name key assoc f =
        case lookup key assoc of
          Just od -> f od
          Nothing -> Message.noSlotName n name

    pos :: [B.TokenTree] -> String -> B.Ab [B.TokenTree]
    pos od "all" = Right od
    pos od n     = case (reads :: ReadS Int) n of
                     [(i, "")] -> Right . B.singleton =<< od `at` i
                     _         -> Message.noSlotName 0 n

    at = slotIndex $ unwords . map B.tokenContent . B.untree

slotIndex :: (a -> String) -> [a] -> Int -> B.Ab a
slotIndex toString xxs n = loop xxs n where
    loop (x : _)  1 = Right x
    loop (_ : xs) i = loop xs $ i - 1
    loop _ _        = Message.noSlotIndex (number $ map toString xxs) n
    number xs = map pair $ zip [1 :: Int ..] xs
    pair (i, x) = "@'" ++ show i ++ " = " ++ x


{-# OPTIONS_GHC -Wall #-}

-- | Slot substitution.

module Koshucode.Baala.Syntax.Attr.Slot
  ( AttrTree,
    GlobalSlot,
    substSlot,
  ) where

import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.TTree           as S
import qualified Koshucode.Baala.Syntax.Token           as S
import qualified Koshucode.Baala.Syntax.Attr.AttrName   as S
import qualified Koshucode.Baala.Syntax.Attr.Message    as Msg

-- | Attribute name and its contents.
type AttrTree = (S.AttrName, [S.TTree])

-- | Global slot name and its content.
type GlobalSlot = S.NamedTrees

-- | Substitute slots by global and attribute slots.
substSlot :: [GlobalSlot] -> [AttrTree] -> B.AbMap [S.TTree]
substSlot gslot attr = Right . concat B.<=< mapM (substTree gslot attr)

substTree :: [GlobalSlot] -> [AttrTree] -> S.TTree -> B.Ab [S.TTree]
substTree gslot attr tree = Msg.abSlotTree tree $ loop tree where
    loop (B.TreeB p q sub) = do sub' <- mapM loop sub
                                Right [B.TreeB p q $ concat sub']
    loop (B.TreeL (S.TSlot _ n name))
        | n == 0    = replace n name S.attrNameTrunk attr (`pos` name)
        | n == 1    = replace n name (S.AttrNormal name) attr Right
        | n == 2    = replace n name name gslot Right
        | otherwise = Msg.noSlotName n name
    loop tk = Right [tk]

    replace n name key assoc f =
        case lookup key assoc of
          Just od -> f od
          Nothing -> Msg.noSlotName n name

    pos :: [S.TTree] -> String -> B.Ab [S.TTree]
    pos od "all" = Right od
    pos od n     = case O.readInt n of
                     Just i  -> Right . B.li1 =<< od `at` i
                     Nothing -> Msg.noSlotName 0 n

    at = substIndex $ unwords . map S.tokenContent . B.untree

substIndex :: (a -> String) -> [a] -> Int -> B.Ab a
substIndex toString xxs n = loop xxs n where
    loop (x : _)  1 = Right x
    loop (_ : xs) i = loop xs $ i - 1
    loop _ _        = Msg.noSlotIndex (number $ map toString xxs) n
    number xs = map pair $ zip [1 :: Int ..] xs
    pair (i, x) = "@'" ++ show i ++ " = " ++ x


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Slot substitution.

module Koshucode.Baala.Syntax.Attr.Slot
  ( AttrTree,
    GlobalSlot,
    substSlot,
  ) where

import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Tree            as S
import qualified Koshucode.Baala.Syntax.Token           as S
import qualified Koshucode.Baala.Syntax.Attr.AttrName   as S
import qualified Koshucode.Baala.Syntax.Pattern         as P
import qualified Koshucode.Baala.Syntax.Attr.Message    as Msg

-- | Attribute name and its contents.
type AttrTree t = (S.AttrName, [S.TTree t])

-- | Global slot name and its content.
type GlobalSlot t = (String, [S.TTree t])

-- | Substitute slots by global and attribute slots.
substSlot :: (O.Textual t) => [GlobalSlot t] -> [AttrTree t] -> B.AbMap [S.TTree t]
substSlot gslot attr = Right . concat O.#. mapM (substTree gslot attr)

substTree :: forall t. (O.Textual t) =>
    [GlobalSlot t] -> [AttrTree t] -> S.TTree t -> B.Ab [S.TTree t]
substTree gslot attr tree = Msg.abSlot [tree] $ loop tree where
    loop (B.TreeB p q sub) = do sub' <- mapM loop sub
                                Right [B.TreeB p q $ concat sub']
    loop (P.LSlot n name) =
        case n of
          S.SlotPos      -> replace n name S.attrNameTrunk attr (`pos` name)
          S.SlotNamed    -> replace n name (S.AttrNormal $ S.tChars name) attr Right
          S.SlotGlobal   -> replace n name (O.tString name) gslot Right
    loop tk = Right [tk]

    replace n name key assoc f =
        case lookup key assoc of
          Just od -> f od
          Nothing -> Msg.noSlotName n name

    pos :: [S.TTree t] -> t -> B.Ab [S.TTree t]
    pos od "all" = Right od
    pos od n     = case O.tInt n of
                     Just i  -> Right . B.list1 O.# od `at` i
                     Nothing -> Msg.noSlotName S.SlotPos n

    at = substIndex $ unwords . map S.tokenContent . B.untree

substIndex :: (a -> String) -> [a] -> Int -> B.Ab a
substIndex toString xxs n = loop xxs n where
    loop (x : _)  1 = Right x
    loop (_ : xs) i = loop xs $ i - 1
    loop _ _        = Msg.noSlotIndex (number $ map toString xxs) n
    number xs = map pair $ zip [1 :: Int ..] xs
    pair (i, x) = "@'" ++ show i ++ " = " ++ x


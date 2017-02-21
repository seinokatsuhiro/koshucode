{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Slot substitution.

module Koshucode.Baala.Syntax.Attr.Slot
  ( GlobalSlot,
    AttrTree,
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

-- | Global slot name and its content.
type GlobalSlot t = (String, [S.TTree t])

-- | Attribute name and its contents.
type AttrTree t = (S.AttrName, [S.TTree t])

-- | Substitute slots by global and attribute slots.
substSlot :: (O.Textual t) => [GlobalSlot t] -> [AttrTree t] -> B.AbMap [S.TTree t]
substSlot global local = Right . concat O.#. mapM (substTree global local)

{-| Substitute slots in tree. -}
substTree :: forall t. (O.Textual t) =>
    [GlobalSlot t] -> [AttrTree t] -> S.TTree t -> B.Ab [S.TTree t]
substTree global local tree = Msg.abSlot tree $ loop tree where
    loop (B.TreeB b y sub) = do sub' <- loop O.<#> sub
                                Right [B.TreeB b y $ concat sub']
    loop (P.LSlot t name)  = case t of
                               S.SlotPos    -> substPos    t name
                               S.SlotNamed  -> substNamed  t name
                               S.SlotGlobal -> substGlobal t name
    loop t = Right [t]

    substGlobal t name = subst t name global (O.tString name)
    substNamed  t name = subst t name local  (S.AttrNormal $ S.tChars name)
    substPos    t name = do ts <- subst t name local S.attrNameTrunk
                            pos t ts name
    subst t name assoc key =
        case lookup key assoc of
          Just ts -> Right ts
          Nothing -> Msg.noSlotName t name

    -- positional slot
    pos :: S.SlotType -> [S.TTree t] -> t -> B.Ab [S.TTree t]
    pos _  ts "all"  = Right ts
    pos ty ts name   = case O.tInt name of
                         Nothing -> Msg.noSlotName ty name
                         Just i  -> case O.lookupIx i ts of
                                      Nothing -> Msg.noSlotName ty name
                                      Just t  -> Right [t]


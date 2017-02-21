{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Slot substitution.

module Koshucode.Baala.Syntax.Attr.Slot
  ( GlobalSlot,
    LocalSlot,
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

-- | Local slot name and its content.
type LocalSlot t = (S.AttrName, [S.TTree t])

-- | Substitute slots by global and attribute slots.
substSlot :: (O.Textual t) => [GlobalSlot t] -> [LocalSlot t] -> B.AbMap [S.TTree t]
substSlot global local = Right . concat O.#. mapM (substTree global local)

{-| Substitute slots in tree. -}
substTree :: forall t. (O.Textual t) =>
    [GlobalSlot t] -> [LocalSlot t] -> S.TTree t -> B.Ab [S.TTree t]
substTree global local tree = Msg.abSlot tree $ loop tree where
    loop (B.TreeB b y sub) = do sub' <- loop O.<#> sub
                                Right [B.TreeB b y $ concat sub']
    loop (P.LSlot ty name) = case ty of
                               S.SlotPos    -> substPos    ty name (O.tInt name)
                               S.SlotNamed  -> substNamed  ty name
                               S.SlotNum i  -> substNamed  ty name O.<||>
                                               substPos    ty name (Just i)
                               S.SlotGlobal -> substGlobal ty name
    loop t = Right [t]

    substGlobal ty name   = subst ty name global (O.tString name)
    substNamed  ty name   = subst ty name local  (S.AttrNormal $ S.tChars name)
    substPos    ty name i = do ts <- subst ty name local S.attrNameTrunk
                               pos ty ts i name

    subst ty name assoc key =
        case lookup key assoc of
          Just ts -> Right ts
          Nothing -> Msg.noSlotName ty name

    -- positional slot
    pos :: S.SlotType -> [S.TTree t] -> Maybe Int -> t -> B.Ab [S.TTree t]
    pos _  ts _ "all"        = Right ts
    pos ty _  Nothing name   = Msg.noSlotName ty name
    pos ty ts (Just i) name  = case O.lookupIx i ts of
                                 Nothing -> Msg.noSlotName ty name
                                 Just t  -> Right [t]

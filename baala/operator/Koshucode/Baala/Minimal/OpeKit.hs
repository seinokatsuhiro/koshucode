{-# OPTIONS_GHC -Wall #-}

-- | Kit for implementing relational operators

module Koshucode.Baala.Minimal.OpeKit
( 
  -- * Monoid
  Monoid.mappend
, Monoid.mconcat

  -- * Abort
, Abort.Abort (..)
, Abort.AbortOr
, (Abort.<!!>)

  -- * Data
, Data.IntValue
, Data.Rel (..)
, Data.RelValue
, Data.Relhead (..)
, Data.RelmapFun
, Data.Relterm (..)
, Data.StringValue
, Data.headFrom
, Data.headPosh
, Data.headPoss
, Data.intValue
, Data.posPoss
, Data.possInner
, Data.possOuter
, Data.possPick
, Data.rehead
, Data.relValue
, Data.reldee
, Data.reldum
, Data.stringValue

  -- * Order
, Order.orders
, Order.sortByName

  -- * Prelude
, Prelude.Listmap
, Prelude.gatherToMap
, Prelude.indexCut
, Prelude.indexPick
, Prelude.lookupMap
, Prelude.unique

  -- * Relmap
, Relmap.OpCons
, Relmap.OpUse (..)
, Relmap.OperandParser
, Relmap.OperandParser'
, Relmap.Relmap (..)
, Relmap.RelmapCons (..)
, Relmap.RelmapFullCons
, Relmap.RelmapHalfCons
, Relmap.RelmapImplement (..)
, Relmap.halfOperand
, Relmap.relmapCalc
, Relmap.relmapConfl
, Relmap.relmapCons
, Relmap.relmapSource

  -- * Section
, Section.QuasiQuoter
, Section.koshuQuoter

  -- * Syntax
, Syntax.Calc
, Syntax.Ripen
, Syntax.Token (..)
, Syntax.TokenTree
, Syntax.Tree (..)
, Syntax.binaryTree
, Syntax.crop
, Syntax.heightTable
, Syntax.operandGroup
, Syntax.termTreePairs

  -- * WithName
, WithName.withN1
, WithName.withP
, WithName.withP2
) where

import Data.Monoid                         as Monoid
import Koshucode.Baala.Base.Data           as Data
import Koshucode.Baala.Base.Kit.Order      as Order
import Koshucode.Baala.Base.Kit.WithName   as WithName
import Koshucode.Baala.Base.Prelude        as Prelude
import Koshucode.Baala.Base.Prelude.Abort  as Abort
import Koshucode.Baala.Base.Relmap         as Relmap
import Koshucode.Baala.Base.Section        as Section
import Koshucode.Baala.Base.Syntax         as Syntax


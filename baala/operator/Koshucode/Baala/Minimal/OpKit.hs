{-# OPTIONS_GHC -Wall #-}

-- | Kit for implementing relational operators

module Koshucode.Baala.Minimal.OpKit
( 
  -- * Monoid
  Monoid.mappend,
  Monoid.mconcat,

  -- * Abort
  Abort.Abort,
  Abort.AbortOr,
  (Abort.<!!>),
  bug,

  -- * Content
  Content.IntValue,
  Content.Nil,
  Content.RelValue,
  Content.StringValue,
  Content.boolValue,
  Content.intValue,
  Content.listValue,
  Content.nil,
  Content.relValue,
  Content.stringValue,

  -- * Data
  Data.Rel (..),
  Data.Relhead (..),
  Data.RelmapFun,
  Data.Relterm (..),
  Data.headFrom,
  Data.headPosh,
  Data.headPoss,
  Data.posPoss,
  Data.possInner,
  Data.possOuter,
  Data.possPick,
  Data.rehead,
  Data.reldee,
  Data.reldum,

  -- * Prelude
  Prelude.Map,
  Prelude.Listmap,
  Prelude.gatherToMap,
  Prelude.indexCut,
  Prelude.indexPick,
  Prelude.lookupMap,
  Prelude.unique,

  -- * Relmap
  Relmap.Relop,
  Relmap.OpImplement (..),
  Relmap.OpParser,
  Relmap.OpParser',
  Relmap.OpUse (..),
  Relmap.Relmap (..),
  Relmap.RelmapCons (..),
  Relmap.RelmapFullCons,
  Relmap.RelmapHalfCons,
  Relmap.halfOperand,
  Relmap.relmapAlias,
  Relmap.relmapCalc,
  Relmap.relmapConfl,
  Relmap.relmapCons,
  Relmap.relmapConst,
  Relmap.relmapSource,

  -- * Section
  Section.Calc,
  Section.QuasiQuoter,
  Section.Ripen,
  Section.crop,
  Section.koshuQuoter,
  Section.operandGroup,
  Section.termNamePairs,
  Section.termNames,
  Section.termTreePairs,

  -- * Syntax
  Syntax.Token (..),
  Syntax.TokenTree,
  Syntax.Tree (..),
  Syntax.binaryTree,
  Syntax.heightTable,
  Syntax.heightTableUnbox,

) where

import Data.Monoid                         as Monoid

import Koshucode.Baala.Base.Abort          as Abort
import Koshucode.Baala.Base.Data           as Data
import Koshucode.Baala.Base.Prelude        as Prelude
import Koshucode.Baala.Base.Relmap         as Relmap
import Koshucode.Baala.Base.Section        as Section
import Koshucode.Baala.Base.Syntax         as Syntax

import Koshucode.Baala.Base.Content.Class as Content


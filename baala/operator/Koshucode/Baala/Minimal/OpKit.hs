{-# OPTIONS_GHC -Wall #-}

{-| Kit for implementing relational operators -}

module Koshucode.Baala.Minimal.OpKit
( 
  -- * Monoid
  Monoid.mappend,
  Monoid.mconcat,

  -- * Abort
  Abort.Abort,
  Abort.AbortOr,
  (Abort.<!!>),
  Abort.bug,

  -- * Content
  Content.CInt,
  Content.CNil,
  Content.CRel,
  Content.CString,
  Content.nil,
  Content.putBool,
  Content.putInt,
  Content.putList,
  Content.putRel,
  Content.putString,

  -- * Data
  Data.Rel (..),
  Data.Relhead (..),
  Data.Relterm (..),
  Data.headChange,
  Data.headFrom,
  Data.headPoss,
  Data.posFrom,
  Data.posPoss,
  Data.possPick,
  Data.reldee,
  Data.reldum,
  Data.termsInner,
  Data.termsOuter,

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
  Section.QuasiQuoter,
  Section.koshuQuoter,
  Section.operandGroup,

  -- * Syntax
  Syntax.Token (..),
  Syntax.TokenTree,
  Syntax.Tree (..),
  Syntax.binaryTree,
  Syntax.heightTable,
  Syntax.heightTableUnbox,

  -- * Term
  Term.termNamePairs,
  Term.termNames,
  Term.termTreePairs,

) where

import Data.Monoid                         as Monoid

import Koshucode.Baala.Base.Abort          as Abort
import Koshucode.Baala.Base.Content        as Content
import Koshucode.Baala.Base.Data           as Data
import Koshucode.Baala.Base.Prelude        as Prelude
import Koshucode.Baala.Base.Relmap         as Relmap
import Koshucode.Baala.Base.Syntax         as Syntax

import Koshucode.Baala.Core.Section        as Section

import Koshucode.Baala.Minimal.Term        as Term


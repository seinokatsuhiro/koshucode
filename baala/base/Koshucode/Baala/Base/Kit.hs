{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC #-}

-- | Kit for implementing relmaps.

module Koshucode.Baala.Base.Kit
( Livmap, Livmap2
, module Data.Monoid
, module Koshucode.Baala.Base.Data.Rel.Relhead
, module Koshucode.Baala.Base.Relmap
, module Koshucode.Baala.Base.Section
, module Koshucode.Baala.Base.Kit.Order
, module Koshucode.Baala.Base.Kit.WithName
, module Koshucode.Baala.Base.Prelude
) where

import Data.Monoid
import Koshucode.Baala.Base.Data.Rel.Relhead
import Koshucode.Baala.Base.Relmap
import Koshucode.Baala.Base.Section
import Koshucode.Baala.Base.Kit.Order
import Koshucode.Baala.Base.Kit.WithName
import Koshucode.Baala.Base.Prelude hiding ((<>), hang, empty)

{-# ANN module "HLint: ignore Use import/export shortcut" #-}

type Livmap a  = [a] -> [a]
type Livmap2 a = forall a. [a] -> [a]


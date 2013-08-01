{-# OPTIONS_GHC -Wall #-}

{-| Relation-to-relation mapping. -}

module Koshucode.Baala.Core.Relmap
( -- * Datatypes
  module Koshucode.Baala.Core.Relmap.HalfRelmap
, module Koshucode.Baala.Core.Relmap.Relmap
, module Koshucode.Baala.Core.Relmap.Assert

  -- * Processes
, module Koshucode.Baala.Core.Relmap.Implement
, module Koshucode.Baala.Core.Relmap.Construct
, module Koshucode.Baala.Core.Relmap.Run

  -- * Datatypes
  -- $Dependencies
) where

import Koshucode.Baala.Core.Relmap.HalfRelmap
import Koshucode.Baala.Core.Relmap.Relmap
import Koshucode.Baala.Core.Relmap.Assert

import Koshucode.Baala.Core.Relmap.Implement
import Koshucode.Baala.Core.Relmap.Construct
import Koshucode.Baala.Core.Relmap.Run



-- ----------------------
-- $Dependencies
--
-- Dependencies of significant datatypes.
-- 
-- * Data 'Assert' has 'Relmap'.
-- 
-- * Data 'Relmap' has
--   'Relmap',
--   'Koshucode.Baala.Base.Data.Rel.Rel', and
--   'HalfRelmap'.
-- 
-- * Data 'HalfRelmap' has
--   'HalfRelmap',
--   'Koshucode.Baala.Base.Abort.CodeLine' and
--   'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
-- 
-- Datatypes for implementing 'Relmap' operators.
--
-- * Data 'Rop' has
--   'RopParser' and
--   'RopCons'.
--
-- * Data 'RopParser' has
--   'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
--
-- * Data 'RopCons' has
--   'RopUse' and
--   'Relmap'.
--
-- * Data 'RopUse' has
--   'HalfRelmap' and
--   'Relmap'.
--
-- Datatypes for constructing 'Relmap'.
-- 
-- * Data 'RelmapCons' has
--   'RelmapHalfCons' and
--   'RelmapFullCons'.
--    
-- * Type 'RelmapHalfCons' has
--   'HalfRelmap',
--   'Koshucode.Baala.Base.Abort.CodeLine' and
--   'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
-- 
-- * Type 'RelmapFullCons' has
--   'HalfRelmap' and
--   'Relmap'.
--


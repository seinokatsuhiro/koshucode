{-# OPTIONS_GHC -Wall #-}

{-| Relation-to-relation mapping. -}

module Koshucode.Baala.Base.Relmap
( -- * Datatypes
  module Koshucode.Baala.Base.Relmap.HalfRelmap
, module Koshucode.Baala.Base.Relmap.Relmap
, module Koshucode.Baala.Base.Relmap.Assert

  -- * Processes
, module Koshucode.Baala.Base.Relmap.Implement
, module Koshucode.Baala.Base.Relmap.Construct
, module Koshucode.Baala.Base.Relmap.Run

  -- * Datatypes
  -- $Dependencies
) where

import Koshucode.Baala.Base.Relmap.Assert
import Koshucode.Baala.Base.Relmap.HalfRelmap
import Koshucode.Baala.Base.Relmap.Implement
import Koshucode.Baala.Base.Relmap.Relmap
import Koshucode.Baala.Base.Relmap.Construct
import Koshucode.Baala.Base.Relmap.Run



-- ----------------------
-- $Dependencies
--
-- Dependencies of significant datatypes.
-- 
-- * Data 'HalfRelmap' has
--   'HalfRelmap',
--   'Koshucode.Baala.Base.Prelude.Abort.SourceLine' and
--   'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
-- 
-- * Data 'Relmap' has
--   'Relmap',
--   'Koshucode.Baala.Base.Data.Rel.Rel', and
--   'HalfRelmap'.
-- 
-- * Data 'Assert' has 'Relmap'.
-- 
-- Datatypes for implementing 'Relmap' operators.
--
-- * Data 'OpImplement' has
--   'OpParser' and
--   'OpCons'.
--
-- * Data 'OpParser' has
--   'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
--
-- * Data 'OpCons' has
--   'OpUse' and
--   'Relmap'.
--
-- Datatypes for constructing 'Relmap'.
--    
-- * Type 'RelmapHalfCons' has
--   'HalfRelmap',
--   'Koshucode.Baala.Base.Prelude.Abort.SourceLine' and
--   'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
-- 
-- * Type 'RelmapFullCons' has
--   'HalfRelmap' and
--   'Relmap'.
-- 
-- * Data 'RelmapCons' has
--   'RelmapHalfCons' and
--   'RelmapFullCons'.
--


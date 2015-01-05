{-# OPTIONS_GHC -Wall #-}

-- | Runtime structure for calculations written in Koshucode.
--   'Resource' is bundle of calculation stuff.  

module Koshucode.Baala.Core.Resource
  ( -- * Modules
    module Koshucode.Baala.Core.Resource.Clause,
    module Koshucode.Baala.Core.Resource.Include,
    module Koshucode.Baala.Core.Resource.Quoter,
    module Koshucode.Baala.Core.Resource.Read,
    module Koshucode.Baala.Core.Resource.Run,
    module Koshucode.Baala.Core.Resource.Resource,
  
    -- * Data dependencies
    -- $DataDependency
  ) where

import Koshucode.Baala.Core.Resource.Clause
import Koshucode.Baala.Core.Resource.Include
import Koshucode.Baala.Core.Resource.Quoter
import Koshucode.Baala.Core.Resource.Read
import Koshucode.Baala.Core.Resource.Run
import Koshucode.Baala.Core.Resource.Resource


-- ----------------------
-- $DataDependency
--
-- Dependencies of significant data types.
--
-- * 'Resource' uses
--   'Resource',
--   'Koshucode.Baala.Core.Relmap.Assert',
--   'Koshucode.Baala.Core.Relmap.Relmap',
--   'Koshucode.Baala.Base.Data.Judge.Judge' and
--   'Koshucode.Baala.Core.Relmap.Construct'.
--
-- * 'Clause' uses
--   'Clause',
--   'Koshucode.Baala.Core.Relmap.Lexmap', and
--   'Koshucode.Baala.Base.Syntax.Token.Token'.
--


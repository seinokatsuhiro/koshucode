{-# OPTIONS_GHC -Wall #-}

-- | Runtime structure for calculations written in Koshucode.
--   'Section' is bundle of calculation stuff.  

module Koshucode.Baala.Core.Section
(
-- * Modules
module Koshucode.Baala.Core.Section.Bundle,
module Koshucode.Baala.Core.Section.Clause,
module Koshucode.Baala.Core.Section.Quoter,
module Koshucode.Baala.Core.Section.Read,
module Koshucode.Baala.Core.Section.Run,
module Koshucode.Baala.Core.Section.Section,

-- * Data dependencies
-- $DataDependency
) where

import Koshucode.Baala.Core.Section.Bundle
import Koshucode.Baala.Core.Section.Clause
import Koshucode.Baala.Core.Section.Quoter
import Koshucode.Baala.Core.Section.Read
import Koshucode.Baala.Core.Section.Run
import Koshucode.Baala.Core.Section.Section


-- ----------------------
-- $DataDependency
--
-- Dependencies of significant data types.
--
-- * 'Section' uses
--   'Section',
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


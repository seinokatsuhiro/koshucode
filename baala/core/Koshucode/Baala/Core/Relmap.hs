{-# OPTIONS_GHC -Wall #-}

-- | Relation-to-relation mapping.

module Koshucode.Baala.Core.Relmap
  ( module Koshucode.Baala.Core.Relmap.Construct,
    module Koshucode.Baala.Core.Relmap.Global,
    module Koshucode.Baala.Core.Relmap.Relkit,
    module Koshucode.Baala.Core.Relmap.Relmap,
    module Koshucode.Baala.Core.Relmap.Rop,
    module Koshucode.Baala.Core.Relmap.Run,
    module Koshucode.Baala.Core.Relmap.Specialize,
  
    -- * Datatypes
    -- $Dependencies
  ) where

import Koshucode.Baala.Core.Relmap.Construct
import Koshucode.Baala.Core.Relmap.Global
import Koshucode.Baala.Core.Relmap.Relkit
import Koshucode.Baala.Core.Relmap.Relmap
import Koshucode.Baala.Core.Relmap.Rop
import Koshucode.Baala.Core.Relmap.Run
import Koshucode.Baala.Core.Relmap.Specialize



-- ----------------------
-- $Dependencies
--
--  Dependencies of significant datatypes.
--
--  * Data 'Relmap' has
--    'Relmap' (recursive),
--    'Koshucode.Baala.Base.Data.Rel.Rel', and
--    'Lexmap'.
--
--  * Data 'Lexmap' has
--    'Lexmap' (recursive),
--    'Koshucode.Baala.Base.Syntax.Tokenize.TokenLine' and
--    'Koshucode.Baala.Base.Syntax.TTree.TTree'.
--
--  Datatypes for implementing 'Relmap' operators.
--
--  * Data 'Rop' has
--    'AttrSort' and
--    'RopCons'.
--
--  * Data 'AttrSort' has
--    'Koshucode.Baala.Base.Syntax.TTree.TTree'.
--
--  * Data 'RopCons' has
--    'RopUse' and
--    'Relmap'.
--
--  * Data 'RopUse' has
--    'Lexmap' and
--    'Relmap'.
--
--  Datatypes for constructing 'Relmap'.
--
--  * Type 'ConsLexmap' has
--    'Lexmap',
--    'Koshucode.Baala.Base.Syntax.Tokenize.TokenLine' and
--    'Koshucode.Baala.Base.Syntax.TTree.TTree'.
--
--  * Type 'ConsRelmap' has
--    'Lexmap' and
--    'Relmap'.
--


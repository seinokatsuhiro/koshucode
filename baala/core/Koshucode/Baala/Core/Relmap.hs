{-# OPTIONS_GHC -Wall #-}

-- | Relation-to-relation mapping.

module Koshucode.Baala.Core.Relmap
(
module Koshucode.Baala.Core.Relmap.Construct,
module Koshucode.Baala.Core.Relmap.Operator,
module Koshucode.Baala.Core.Relmap.Relkit,
module Koshucode.Baala.Core.Relmap.Run,
module Koshucode.Baala.Core.Relmap.Specialize,

-- * Datatypes
-- $Dependencies
) where

import Koshucode.Baala.Core.Relmap.Construct
import Koshucode.Baala.Core.Relmap.Operator
import Koshucode.Baala.Core.Relmap.Relkit
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
--    'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
--
--  Datatypes for implementing 'Relmap' operators.
--
--  * Data 'Rop' has
--    'RodSorter' and
--    'RopCons'.
--
--  * Data 'RodSorter' has
--    'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
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
--  * Data 'RelmapCons' has
--    'ConsLexmap' and
--    'ConsRelmap'.
--
--  * Type 'ConsLexmap' has
--    'Lexmap',
--    'Koshucode.Baala.Base.Syntax.Tokenize.TokenLine' and
--    'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
--
--  * Type 'ConsRelmap' has
--    'Lexmap' and
--    'Relmap'.
--


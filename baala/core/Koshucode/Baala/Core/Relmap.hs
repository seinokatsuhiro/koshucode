{-# OPTIONS_GHC -Wall #-}

{-| Relation-to-relation mapping. -}

module Koshucode.Baala.Core.Relmap
( module Koshucode.Baala.Core.Relmap.Construct,
  module Koshucode.Baala.Core.Relmap.Lexical,
  module Koshucode.Baala.Core.Relmap.Operand,
  module Koshucode.Baala.Core.Relmap.Relkit,
  module Koshucode.Baala.Core.Relmap.Relmap,
  module Koshucode.Baala.Core.Relmap.Rop,

  -- * Datatypes
  -- $Dependencies
) where

import Koshucode.Baala.Core.Relmap.Construct
import Koshucode.Baala.Core.Relmap.Lexical
import Koshucode.Baala.Core.Relmap.Operand
import Koshucode.Baala.Core.Relmap.Relkit
import Koshucode.Baala.Core.Relmap.Relmap
import Koshucode.Baala.Core.Relmap.Rop



-- ----------------------
{- $Dependencies

 Dependencies of significant datatypes.
 
 * Data 'Relmap' has
   'Relmap' (recursive),
   'Koshucode.Baala.Base.Data.Rel.Rel', and
   'LexRelmap'.
 
 * Data 'LexRelmap' has
   'LexRelmap' (recursive),
   'Koshucode.Baala.Base.Syntax.Tokenize.TokenLine' and
   'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
 
 Datatypes for implementing 'Relmap' operators.

 * Data 'Rop' has
   'RopFullSorter' and
   'RopCons'.

 * Data 'RopFullSorter' has
   'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.

 * Data 'RopCons' has
   'RopUse' and
   'Relmap'.

 * Data 'RopUse' has
   'LexRelmap' and
   'Relmap'.

 Datatypes for constructing 'Relmap'.
 
 * Data 'RelmapCons' has
   'RelmapConsLex' and
   'RelmapConsFull'.
    
 * Type 'RelmapConsLex' has
   'LexRelmap',
   'Koshucode.Baala.Base.Syntax.Tokenize.TokenLine' and
   'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
 
 * Type 'RelmapConsFull' has
   'LexRelmap' and
   'Relmap'.
-}


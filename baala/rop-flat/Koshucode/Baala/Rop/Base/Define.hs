{-# OPTIONS_GHC -Wall #-}

-- | Definition of relmap operators.

module Koshucode.Baala.Rop.Base.Define
  ( RopDefine,
    ropList, rop,
    ropAlias,
  ) where

import qualified Koshucode.Baala.Overture   as O
import qualified Koshucode.Baala.Syntax     as S
import qualified Koshucode.Baala.Core       as C

-- | Constructor, usage, and attribute sorter
{-# WARNING RopDefine "This is only used in defined module." #-}
type RopDefine c = (C.RopCons c, S.AttrLayout)

-- | Make implementations of relmap operators.
ropList
    :: C.RopGroup     -- ^ Operator group
    -> [RopDefine c]  -- ^ Operator definitions
    -> [C.Rop c]      -- ^ Relmap operators
ropList group = map make where
    make (cons, layout) =
        let usage  = S.attrUsageString layout
            name   = head $ words usage
            sorter = S.attrParaBy layout
        in C.Rop { C.ropName   = name
                 , C.ropGroup  = group
                 , C.ropAttr   = layout
                 , C.ropParaze = sorter
                 , C.ropCons   = cons }

-- | Make definition of relmap operator.
rop :: C.RopCons c              -- ^ Constructor
    -> [(S.AttrUsage, String)]  -- ^ Usage text and attribute layout
    -> RopDefine c              -- ^ Operator definition
rop cons ul = (cons, S.toAttrLayout ul)

-- | Add aliases of relmap operator.
ropAlias
    :: [(C.RopName, C.RopName)]  -- ^ Alias and original rop names
    -> O.Map [C.Rop c]           -- ^ Original rop list to add aliases.
ropAlias alias rops = foldr ropAliasAdd rops alias

-- | Add alias of relmap operator.
ropAliasAdd :: (C.RopName, C.RopName) -> O.Map [C.Rop c]
ropAliasAdd (name, orig) = loop where
    loop [] = []
    loop (r : rs) | C.ropName r == orig  = r { C.ropName = name } : r : rs
                  | otherwise            = r : loop rs
    

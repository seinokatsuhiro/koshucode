{-# OPTIONS_GHC -Wall #-}

-- | Definition of relmap operators.

module Koshucode.Baala.Rop.Base.Define
  ( rops,
    ropAlias,
  ) where

import qualified Koshucode.Baala.Overture   as O
import qualified Koshucode.Baala.Syntax     as S
import qualified Koshucode.Baala.Core       as C

-- | Make implementations of relmap operators.
rops :: C.RopGroup     -- ^ Operator group
     -> [(C.RopCons c, [(S.AttrUsage, String)])]
                       -- ^ Constructor and list of usage and layout
     -> [C.Rop c]      -- ^ Operator list
rops group def = make <$> (S.toAttrLayout O.<$$> def) where
    make (cons, layout) =
        let usage  = S.attrUsageString layout
            name   = head $ words usage
            sorter = S.attrParaBy layout
        in C.Rop { C.ropName   = name
                 , C.ropGroup  = group
                 , C.ropAttr   = layout
                 , C.ropParaze = sorter
                 , C.ropCons   = cons }

-- | Add aliases of relmap operator.
ropAlias
    :: [(C.RopName, C.RopName)]  -- ^ Alias and original rop names
    -> O.Map [C.Rop c]           -- ^ Original rop list to add aliases.
ropAlias alias rs = foldr ropAliasAdd rs alias

-- | Add alias of relmap operator.
ropAliasAdd :: (C.RopName, C.RopName) -> O.Map [C.Rop c]
ropAliasAdd (name, orig) = loop where
    loop [] = []
    loop (r : rs) | C.ropName r == orig  = r { C.ropName = name } : r : rs
                  | otherwise            = r : loop rs
    

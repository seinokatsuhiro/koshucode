{-# OPTIONS_GHC -Wall #-}

-- | Definition of relmap operators.

module Koshucode.Baala.Rop.Base.Define
  ( RopDefine,
    ropList, def,
    ropAlias,
  ) where

import qualified Koshucode.Baala.Overture   as O
import qualified Koshucode.Baala.Syntax     as S
import qualified Koshucode.Baala.Core       as C

-- | Constructor, usage, and attribute sorter
type RopDefine c = (C.RopCons c, C.RopUsage, S.AttrLayout)

-- | Make implementations of relmap operators.
ropList
    :: String         -- ^ Operator group
    -> [RopDefine c]  -- ^ Operator definitions
    -> [C.Rop c]      -- ^ Relmap operators
ropList group = map rop where
    rop (cons, usage, attr) =
        let name   = head $ words usage
            sorter = S.attrParaBy attr
        in C.Rop name group usage attr sorter cons

-- | Make definition of relmap operator.
def :: C.RopCons c    -- ^ Constructor
    -> C.RopUsage     -- ^ Rop usage
    -> String         -- ^ Attribute layout
    -> RopDefine c    -- ^ Operator definition
def cons usage attr = (cons, usage, S.parseAttrLayout attr)

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
    

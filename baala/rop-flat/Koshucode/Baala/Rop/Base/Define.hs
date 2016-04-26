{-# OPTIONS_GHC -Wall #-}

-- | Definition of relmap operators.

module Koshucode.Baala.Rop.Base.Define
  ( RopDefine,
    ropList, def,
  ) where

import qualified Koshucode.Baala.Core as C

-- | Constructor, usage, and attribute sorter
type RopDefine c = (C.RopCons c, C.RopUsage, C.AttrSorter)

-- | Make implementations of relmap operators.
ropList
    :: String         -- ^ Operator group
    -> [RopDefine c]  -- ^ Operator definitions
    -> [C.Rop c]      -- ^ Relmap operators
ropList group = map rop where
    rop (cons, usage, attr) =
        let name   = head $ words usage
            sorter = C.attrSort attr
        in C.Rop name group usage attr sorter cons

-- | Make definition of relmap operator.
def :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
def cons usage attr = (cons, usage, C.parseAttrSorter attr)


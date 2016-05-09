{-# OPTIONS_GHC -Wall #-}

-- | Definition of relmap operators.

module Koshucode.Baala.Rop.Base.Define
  ( RopDefine,
    ropList, def,
  ) where

import qualified Koshucode.Baala.Syntax  as S
import qualified Koshucode.Baala.Core    as C

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
            sorter = S.attrParaSort attr
        in C.Rop name group usage attr sorter cons

-- | Make definition of relmap operator.
def :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
def cons usage attr = (cons, usage, snd $ head $ S.parseAttrLayout attr)


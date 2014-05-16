{-# OPTIONS_GHC -Wall #-}

-- | Built-in relmap operator.

module Koshucode.Baala.Op.Builtin.Rop
( builtinRops,
  ropList,
  -- $ListOfOperator
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C


-- | Built-in relmap operator.
builtinRops :: [C.Rop c]
builtinRops = ropList "builtin"
    [ ("append R ...", ropConsConcat, C.roaList "-relmap" []) ]

-- TODO
ropConsConcat :: C.RopCons c
ropConsConcat = Right . foldl B.mappend B.mempty . C.ropSubrelmap

-- | Make implementations of relation-mapping operators.
ropList
    :: String      -- ^ Operator group
    -> [(String, C.RopCons c, C.RoaSpec)]
                   -- ^ Synopsis, constructor, and attribute sorter
    -> [C.Rop c]   -- ^ List of relation-mapping operators
ropList group = map rop where
    rop :: (String, C.RopCons c, C.RoaSpec) -> C.Rop c
    rop (usage, cons, roa) =
        let name   = head $ words usage
            sorter = C.roaSorter roa
        in C.Rop name group sorter cons usage


-- ----------------------
-- $ListOfOperator
--
--  [/r/ @|@ /s/]   Append relmaps


{-# OPTIONS_GHC -Wall #-}

-- | Built-in relmap operator.

module Koshucode.Baala.Op.Builtin.Rop
( builtinRops,
  ropList,
  -- $ListOfOperator
) where

import qualified Data.Monoid as M
import qualified Koshucode.Baala.Core as C


-- | Built-in relmap operator.
builtinRops :: [C.Rop c]
builtinRops = ropList "builtin"
    [ ("append R ...", ropConsConcat, C.rodList "-relmap" []) ]

-- TODO
ropConsConcat :: C.RopCons c
ropConsConcat = Right . foldl M.mappend M.mempty . C.ropSubrelmap
--ropConsConcat = Right . M.mconcat . C.ropSubrelmap

-- | Make implementations of relation-mapping operators.
ropList
    :: String      -- ^ Operator group
    -> [(String, C.RopCons c, C.RodSpec)]
                   -- ^ Synopsis, constructor, and operand sorter
    -> [C.Rop c]   -- ^ List of relation-mapping operators
ropList group = map rop where
    rop :: (String, C.RopCons c, C.RodSpec) -> C.Rop c
    rop (usage, cons, rod) =
        let name   = head $ words usage
            sorter = C.rodSorter rod
        in C.Rop name group sorter cons usage


-- ----------------------
-- $ListOfOperator
--
--  [/r/ @|@ /s/]   Append relmaps


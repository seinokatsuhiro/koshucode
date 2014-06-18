{-# OPTIONS_GHC -Wall #-}

-- | Built-in relmap operator.

module Koshucode.Baala.Op.Builtin.Rop
( ropsBuiltin,
  ropList,

  -- * id
  consId, relmapId,
  -- $id
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C


-- | Built-in relmap operator.
--
--   [/r/ @|@ /s/]   Append relmaps
--
--   [@id@]          Identity relmap
--
ropsBuiltin :: [C.Rop c]
ropsBuiltin = ropList "builtin"
    [ ( "append R ..." , consAppend  , C.roaList "-relmap" [] )
    , ( "id"           , consId      , C.roaNone [] )
    ]


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


-- ----------------------  append

consAppend :: C.RopCons c
consAppend = Right . foldl B.mappend B.mempty . C.ropSubrelmap


-- ----------------------  id

-- $id
--
--  Identity mapping, i.e., do nothing.
--
--    > pick /a /b | id

consId :: C.RopCons c
consId use = Right $ relmapId use

relmapId :: C.RopUse c -> C.Relmap c
relmapId use = C.relmapFlow use $ Right . C.relkitId


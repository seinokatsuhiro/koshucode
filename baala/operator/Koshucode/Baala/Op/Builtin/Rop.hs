{-# OPTIONS_GHC -Wall #-}

-- | Built-in relmap operator.

module Koshucode.Baala.Op.Builtin.Rop
( ropsBuiltin,
  -- * id
  consId, relmapId,
  -- $id
) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Op.Builtin.Define as Op


-- | Built-in relmap operator.
--
--   [/r/ @|@ /s/]   Append relmaps
--
--   [@id@]          Identity relmap
--
ropsBuiltin :: [C.Rop c]
ropsBuiltin = Op.ropList "builtin"
    --        CONSTRUCTOR  USAGE            ATTRIBUTE
    [ Op.ropV consAppend   "append R ..."   "-relmap/"
    , Op.ropN consId       "id"             ""
    ]


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


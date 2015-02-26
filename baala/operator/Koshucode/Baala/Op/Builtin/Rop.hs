{-# OPTIONS_GHC -Wall #-}

-- | Built-in relmap operator.

module Koshucode.Baala.Op.Builtin.Rop
  ( ropsBuiltin,
    -- * id
    consId, relmapId,
    -- $id

    -- * xxx
    consXxx
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Op.Builtin.Define as Op
import qualified Koshucode.Baala.Op.Message        as Msg


-- | Built-in relmap operator.
--
--   [/r/ @|@ /s/]   Append relmaps
--
--   [@id@]          Identity relmap
--
ropsBuiltin :: [C.Rop c]
ropsBuiltin = Op.ropList "builtin"
    --       CONSTRUCTOR  USAGE            ATTRIBUTE
    [ Op.def consAppend   "append R ..."   "V -relmap/"
    , Op.def consId       "id"             "0"
    ]


-- ----------------------  append

consAppend :: C.RopCons c
consAppend = Right . foldl B.mappend B.mempty . C.ropSubmap


-- ----------------------  id

-- $id
--
--  Identity mapping, i.e., do nothing.
--
--    > pick /a /b | id

consId :: C.RopCons c
consId use = Right $ relmapId use

relmapId :: C.Intmed c -> C.Relmap c
relmapId use = C.relmapFlow use $ Right . C.relkitId


-- ---------------------- xxx

-- | Placeholder for unimplemented operator.
consXxx :: C.RopCons c
consXxx _ = Msg.notImpl


{-# OPTIONS_GHC -Wall #-}

-- | Built-in relmap operator.

module Koshucode.Baala.Rop.Base.Rop
  ( ropsBuiltin,
    -- * id
    consId, relmapId,
    -- $id

    -- * xxx
    consXxx
  ) where

import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base.Define    as Op
import qualified Koshucode.Baala.Rop.Base.Message   as Msg


-- | Built-in relmap operator.
--
--   [/r/ @|@ /s/]   Append relmaps
--
--   [@id@]          Identity relmap
--
ropsBuiltin :: [C.Rop c]
ropsBuiltin = Op.ropList "builtin"
    --       CONSTRUCTOR  USAGE            ATTRIBUTE
    [ Op.def consAppend   "append R R"     "-left/ -right/"
    , Op.def consId       "id"             ""
    ]


-- ----------------------  append

--consConcat :: C.RopCons c
--consConcat = Right . foldl B.mappend B.mempty . C.medSubmap

consAppend :: C.RopCons c
consAppend = app . map snd . C.medSubmap where
    app [a,b] = Right $ B.mappend a b
    app _     = Msg.reqRelmap 2


-- ----------------------  id

-- $id
--
--  Identity mapping, i.e., do nothing.
--
--    > pick /a /b | id

consId :: C.RopCons c
consId med = Right $ relmapId med

relmapId :: C.Intmed c -> C.Relmap c
relmapId med = C.relmapFlow med $ Right . C.relkitId


-- ---------------------- xxx

-- | Placeholder for unimplemented operator.
consXxx :: C.RopCons c
consXxx _ = Msg.notImpl


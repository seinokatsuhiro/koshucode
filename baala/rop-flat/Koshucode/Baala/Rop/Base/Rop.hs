{-# OPTIONS_GHC -Wall #-}

-- | Built-in relmap operator.

module Koshucode.Baala.Rop.Base.Rop
  ( ropsBuiltin,
    -- * id
    consId, relmapId,
    -- * xxx
    consXxx
  ) where

import qualified Koshucode.Baala.Overture           as O
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base.Define    as Rop
import qualified Koshucode.Baala.Rop.Base.Message   as Msg

-- | Built-in relmap operator.
--
--   [/r/ @|@ /s/]   Append relmaps
--
--   [@id@]          Identity relmap
--
ropsBuiltin :: [C.Rop c]
ropsBuiltin = Rop.ropList "builtin"
    --        CONSTRUCTOR    USAGE               ATTRIBUTE
    [ Rop.rop consAppend   [ "append R R"    O.& "-left/ -right/" ]
    , Rop.rop consId       [ "id"            O.& "" ]
    ]

-- | Append relmaps.
consAppend :: C.RopCons c
consAppend = app . map snd . C.medSubmap where
    app [a,b] = Right (a O.++ b)
    app _     = Msg.reqRelmap 2

-- | __id__
--
--   Identity relmap, i.e., output just input relation.
--
consId :: C.RopCons c
consId med = Right $ relmapId med

-- | Create @id@ relmap.
relmapId :: C.Intmed c -> C.Relmap c
relmapId med = C.relmapFlow med $ Right . C.relkitId

-- | Placeholder for unimplemented operator.
consXxx :: C.RopCons c
consXxx _ = Msg.notImpl


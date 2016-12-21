{-# OPTIONS_GHC -Wall #-}

-- | Built-in relmap operator.

module Koshucode.Baala.Rop.Base.Rop
  ( ropsBuiltin,
    -- * @id@
    consId, relmapId,
    -- * Append
    consAppend,
    -- * Unimplemented
    consXxx
  ) where

import qualified Koshucode.Baala.Overture           as O
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base.Define    as Rop
import qualified Koshucode.Baala.Rop.Base.Message   as Msg

-- | Built-in relmap operators.
ropsBuiltin :: [C.Rop c]
ropsBuiltin = Rop.rops "builtin"
    [ consAppend O.& [ "append R R" O.& "-left/ -right/" ]
    , consId     O.& [ "id"         O.& "" ]
    ]

-- | [id] Identity relmap, i.e., output just input relation.
consId :: C.RopCons c
consId med = Right $ relmapId med

-- | Create @id@ relmap.
relmapId :: C.Intmed c -> C.Relmap c
relmapId med = C.relmapFlow med $ Right . C.relkitId

-- | [/R/ | /S/] Append two relmaps.
consAppend :: C.RopCons c
consAppend = app . map snd . C.medSubmap where
    app [a,b] = Right (a O.++ b)
    app _     = Msg.reqRelmap 2

-- | Placeholder for unimplemented operator.
consXxx :: C.RopCons c
consXxx _ = Msg.notImpl


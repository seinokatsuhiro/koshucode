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

import qualified Koshucode.Baala.DataPlus           as K
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base.Define    as Rop
import qualified Koshucode.Baala.Rop.Base.Message   as Msg

-- | Built-in relmap operators.
ropsBuiltin :: [C.Rop c]
ropsBuiltin = Rop.rops "builtin"
    [ consAppend K.& [ "append R R" K.& "-left/ -right/" ]
    , consId     K.& [ "id"         K.& "" ]
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
    app [a,b] = Right (a K.++ b)
    app _     = Msg.reqRelmap 2

-- | Placeholder for unimplemented operator.
consXxx :: C.RopCons c
consXxx _ = Msg.notImpl


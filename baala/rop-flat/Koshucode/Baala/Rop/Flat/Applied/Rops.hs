{-# OPTIONS_GHC -Wall #-}

-- | Applied operators.

module Koshucode.Baala.Rop.Flat.Applied.Rops
  ( ropsApplied,
  ) where

import qualified Koshucode.Baala.DataPlus                  as K
import qualified Koshucode.Baala.Core                      as C
import qualified Koshucode.Baala.Rop.Base                  as Rop
import qualified Koshucode.Baala.Rop.Flat.Applied.Gadget   as Rop
import qualified Koshucode.Baala.Rop.Flat.Applied.Subtext  as Rop


-- | Applied operators.
ropsApplied :: (K.CContent c) => [C.Rop c]
ropsApplied = Rop.rops "gadget"
    [ Rop.consContents       K.& [ "contents /N"
                                   K.& "-term*" ]
    , Rop.consDumpTree       K.& [ "dump-tree X"
                                   K.& "-tree*" ]
    , Rop.consEqlize         K.& [ "eqlize"
                                   K.& "" ]
    , Rop.consPoDepth        K.& [ "partial-order-depth /P /P -to /N /N"
                                   K.& "-x -y . -to" ]
    , Rop.consPoHeight       K.& [ "partial-order-height /P /P -to /N /N"
                                   K.& "-x -y . -to" ]
    , Rop.consVisitDistance  K.& [ "visit-distance R -step /P ... -to /N -distance /N"
                                   K.& "-relmap/ . -step -to -distance" ]
    , Rop.consSize           K.& [ "size /N"
                                   K.& "-term" ]
    , Rop.consSubtext        K.& [ "subtext /N E"
                                   K.& "-term -subtext . -trim?" ]
    ]


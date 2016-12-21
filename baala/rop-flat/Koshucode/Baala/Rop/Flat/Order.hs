{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators which produce ordered relations.

module Koshucode.Baala.Rop.Flat.Order
  ( ropsOrder,
    -- * forward
    consForward, relmapForward,
    -- * backward
    consBackward, relmapBackward,
    -- * lexical
    consLexical, relmapLexical,
    -- * order
    consOrder, relmapOrder, relkitOrder,
  ) where

import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Flat.Message  as Msg


-- | Pseudorelmap operators for term and tuple ordering.
ropsOrder :: (Ord c) => [C.Rop c]
ropsOrder = Rop.ropAlias
    [ "fw" K.& "forward"
    , "bw" K.& "backward"
    ] $ Rop.rops "order"
    [ consBackward   K.& [ "backward /P ..."  K.& "-term*" ]
    , consForward    K.& [ "forward /P ..."   K.& "-term*" ]
    , consLexical    K.& [ "lexical"          K.& "" ]
    , consOrder      K.& [ "order /P ..."     K.& "-term*" ]
    ]


-- ----------------------  forward & backward

-- | __forward \/P ...__
--
--   Move terms \/P ... forward.
--
consForward :: C.RopCons c
consForward med =
  do ns <- Rop.getTerms med "-term"
     Right $ relmapForward med ns

-- | Create @forward@ relmap.
relmapForward :: C.Intmed c -> [K.TermName] -> C.Relmap c
relmapForward med = C.relmapFlow med . relkitToward K.forwardTerms2

-- | __backward \/P ...__
--
--   Move terms \/P ... backward.
--
consBackward :: C.RopCons c
consBackward med =
  do ns <- Rop.getTerms med "-term"
     Right $ relmapBackward med ns

-- | Create @backward@ relmap.
relmapBackward :: C.Intmed c -> [K.TermName] -> C.Relmap c
relmapBackward med = C.relmapFlow med . relkitToward K.backwardTerms2

-- | Create @forward@ or @backward@ relkit.
relkitToward :: K.TermPick2 K.TypeTerm c -> [K.TermName] -> C.RelkitFlow c
relkitToward _ _ Nothing = Right C.relkitNothing
relkitToward (hePick, boPick) ns (Just he1)
    | K.newTermsExist pk   = Msg.newTerm pk he1
    | otherwise            = Right kit2
    where
      pk    = K.termPicker ns he1
      he2   = K.headMap (hePick pk) he1
      kit2  = C.relkitJust he2 $ C.RelkitLinear False $ boPick pk


-- ----------------------  lexical

-- | __lexical__
consLexical :: C.RopCons c
consLexical med = Right $ relmapLexical med

-- | Create @lexical@ relmap.
relmapLexical :: C.Intmed c -> C.Relmap c
relmapLexical med = C.relmapFlow med relkitLexical

-- | Create @lexical@ relkit.
relkitLexical :: C.RelkitFlow c
relkitLexical Nothing = Right C.relkitNothing
relkitLexical (Just he1) = Right kit2 where
    ns    = K.getTermNames he1
    pk    = K.termPicker (K.sort ns) ns
    fw    = K.forwardTerms pk
    he2   = K.headMap fw he1
    kit2  = C.relkitJust he2 $ C.RelkitLinear False fw


-- ----------------------  order

-- | __order \/P ...__
consOrder :: (Ord c) => C.RopCons c
consOrder med =
    do ns <- Rop.getOption [] Rop.getTerms med "-term"
       Right $ relmapOrder med ns

-- | Create @order@ relmap.
relmapOrder :: (Ord c) => C.Intmed c -> [K.TermName] -> C.Relmap c
relmapOrder med = C.relmapFlow med . relkitOrder

-- | Create @order@ relkit.
relkitOrder :: (Ord c) => [K.TermName] -> C.RelkitFlow c
relkitOrder _ Nothing = Right C.relkitNothing
relkitOrder ns (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitFull False kitf2
    kitf2 = K.relBodyOrder ns he1


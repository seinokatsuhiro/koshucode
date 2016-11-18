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

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S
import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Flat.Message  as Msg


-- | Pseudorelmap operators for term and tuple ordering.
ropsOrder :: (Ord c) => [C.Rop c]
ropsOrder = Rop.ropList "order"  -- GROUP
    --         CONSTRUCTOR        USAGE                      ATTRIBUTE
    [ Rop.def  consBackward       "backward /P ..."          "-term*"
    , Rop.def  consForward        "forward /P ..."           "-term*"
    , Rop.def  consLexical        "lexical"                  ""
    , Rop.def  consOrder          "order /P ..."             "-term*"
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
relmapForward :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapForward med = C.relmapFlow med . relkitToward (D.ssRForward, D.ssRForward)

-- | __backward \/P ...__
--
--   Move terms \/P ... backward.
--
consBackward :: C.RopCons c
consBackward med =
  do ns <- Rop.getTerms med "-term"
     Right $ relmapBackward med ns

-- | Create @backward@ relmap.
relmapBackward :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapBackward med = C.relmapFlow med . relkitToward (D.ssRBackward, D.ssRBackward)

-- | Create @forward@ or @backward@ relkit.
relkitToward :: D.TermPick2 D.TermType c -> [S.TermName] -> C.RelkitFlow c
relkitToward _ _ Nothing = Right C.relkitNothing
relkitToward (hePick, boPick) ns (Just he1)
    | D.newTermsExist pk   = Msg.unkTerm (D.newTerms pk) he1
    | otherwise            = Right kit2
    where
      pk    = D.termPicker ns he1
      he2   = D.headMap (hePick pk) he1
      kit2  = C.relkitJust he2 $ C.RelkitOneToOne False $ boPick pk


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
    ns    = D.getTermNames he1
    lr    = D.termPicker (B.sort ns) ns
    he2   = D.headMap (D.ssRForward lr) he1
    kit2  = C.relkitJust he2 $ C.RelkitOneToOne False $ D.ssRForward lr


-- ----------------------  order

-- | __order \/P ...__
consOrder :: (Ord c) => C.RopCons c
consOrder med =
    do ns <- Rop.getOption [] Rop.getSignedTerms med "-term"
       Right $ relmapOrder med ns

-- | Create @order@ relmap.
relmapOrder :: (Ord c) => C.Intmed c -> [S.SignedTermName] -> C.Relmap c
relmapOrder med = C.relmapFlow med . relkitOrder

-- | Create @order@ relkit.
relkitOrder :: (Ord c) => [S.SignedTermName] -> C.RelkitFlow c
relkitOrder _ Nothing = Right C.relkitNothing
relkitOrder ns (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitFull False kitf2
    kitf2 = D.relBodyOrder ns he1


{-# OPTIONS_GHC -Wall #-}

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
import qualified Koshucode.Baala.Rop.Base          as Op
import qualified Koshucode.Baala.Rop.Flat.Message  as Msg


-- | Pseudorelmap operators for term and tuple ordering.
ropsOrder :: (Ord c) => [C.Rop c]
ropsOrder = Op.ropList "order"  -- GROUP
    --         CONSTRUCTOR        USAGE                      ATTRIBUTE
    [ Op.def   consBackward       "backward /P ..."          "-term*"
    , Op.def   consForward        "forward /P ..."           "-term*"
    , Op.def   consLexical        "lexical"                  ""
    , Op.def   consOrder          "order /P ..."             "-term*"
    ]


-- ----------------------  forward & backward

consForward :: C.RopCons c
consForward med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapForward med ns

relmapForward :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapForward med = C.relmapFlow med . relkitToward (D.ssRForward, D.ssRForward)

consBackward :: C.RopCons c
consBackward med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapBackward med ns

relmapBackward :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapBackward med = C.relmapFlow med . relkitToward (D.ssRBackward, D.ssRBackward)

-- | Create @forward@ or @backward@ relkit.
relkitToward :: D.ShareSideMap2 D.NamedType c -> [S.TermName] -> C.RelkitFlow c
relkitToward _ _ Nothing = Right C.relkitNothing
relkitToward (heMap, boMap) ns (Just he1)
    | null unk   = Right kit2
    | otherwise  = Msg.unkTerm unk he1
    where
      lr    = D.shareSide ns he1
      unk   = D.ssLSide lr ns
      he2   = D.headMap (heMap lr) he1
      kit2  = C.relkitJust he2 $ C.RelkitOneToOne False $ boMap lr


-- ----------------------  lexical

consLexical :: C.RopCons c
consLexical med = Right $ relmapLexical med

relmapLexical :: C.Intmed c -> C.Relmap c
relmapLexical med = C.relmapFlow med relkitLexical

-- | Create @lexical@ relkit.
relkitLexical :: C.RelkitFlow c
relkitLexical Nothing = Right C.relkitNothing
relkitLexical (Just he1) = Right kit2 where
    ns    = D.getTermNames he1
    lr    = D.shareSide (B.sort ns) ns
    he2   = D.headMap (D.ssRForward lr) he1
    kit2  = C.relkitJust he2 $ C.RelkitOneToOne False $ D.ssRForward lr


-- ----------------------  order

consOrder :: (Ord c) => C.RopCons c
consOrder med =
    do ns <- Op.getOption [] Op.getSignedTerms med "-term"
       Right $ relmapOrder med ns

relmapOrder :: (Ord c) => C.Intmed c -> [S.SignedTermName] -> C.Relmap c
relmapOrder med = C.relmapFlow med . relkitOrder

-- | Create @order@ relkit.
relkitOrder :: (Ord c) => [S.SignedTermName] -> C.RelkitFlow c
relkitOrder _ Nothing = Right C.relkitNothing
relkitOrder ns (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitFull False kitf2
    kitf2 = D.relBodyOrder ns he1


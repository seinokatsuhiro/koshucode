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
    [ Op.def   consBackward       "backward /P ..."          "V -term"
    , Op.def   consForward        "forward /P ..."           "V -term"
    , Op.def   consLexical        "lexical"                  "0"
    , Op.def   consOrder          "order /P ..."             "V -term"
    ]


-- ----------------------  forward & backward

consForward :: C.RopCons c
consForward med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapForward med ns

relmapForward :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapForward med = C.relmapFlow med . relkitToward (D.headRForward, D.headRForward)

consBackward :: C.RopCons c
consBackward med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapBackward med ns

relmapBackward :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapBackward med = C.relmapFlow med . relkitToward (D.headRBackward, D.headRBackward)

relkitToward :: D.HeadLRMap2 D.NamedType c -> [S.TermName] -> C.RelkitFlow c
relkitToward _ _ Nothing = Right C.relkitNothing
relkitToward (heMap, boMap) ns (Just he1)
    | null unk   = Right kit2
    | otherwise  = Msg.unkTerm unk he1
    where
      lr    = ns `D.headLR` D.headNames he1
      unk   = D.headLSide lr ns
      he2   = D.headMap (heMap lr) he1
      kit2  = C.relkitJust he2 $ C.RelkitOneToOne False $ boMap lr


-- ----------------------  lexical

consLexical :: C.RopCons c
consLexical med = Right $ relmapLexical med

relmapLexical :: C.Intmed c -> C.Relmap c
relmapLexical med = C.relmapFlow med relkitLexical

relkitLexical :: C.RelkitFlow c
relkitLexical Nothing = Right C.relkitNothing
relkitLexical (Just he1) = Right kit2 where
    ns    = D.headNames he1
    lr    = B.sort ns `D.headLR` ns
    he2   = D.headMap (D.headRForward lr) he1
    kit2  = C.relkitJust he2 $ C.RelkitOneToOne False $ D.headRForward lr


-- ----------------------  order

consOrder :: (Ord c) => C.RopCons c
consOrder med =
    do ns <- Op.getOption [] Op.getSignedTerms med "-term"
       Right $ relmapOrder med ns

relmapOrder :: (Ord c) => C.Intmed c -> [S.SignedTermName] -> C.Relmap c
relmapOrder med = C.relmapFlow med . relkitOrder

relkitOrder :: (Ord c) => [S.SignedTermName] -> C.RelkitFlow c
relkitOrder _ Nothing = Right C.relkitNothing
relkitOrder ns (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitFull False kitf2
    kitf2 = D.relBodyOrder ns he1


{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Run section.
module Koshucode.Baala.Core.Section.Run
( runSection,
) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Section.Section as C



-- --------------------------------------------  Run

runSection :: (C.CContent c) => C.Global c -> [C.Section c] -> B.Ab (B.OutputResult c)
runSection global sects =
    do s2 <- assembleRelmap $ B.mconcat sects
       let g2 = global { C.globalJudges = C.secJudge s2 }
       runSectionBody g2 s2

runSectionBody :: forall c. (Ord c, B.Pretty c, C.CRel c, C.CNil c) =>
    C.Global c -> C.Section c -> B.Ab (B.OutputResult c)
runSectionBody global C.Section { C.secAssert = ass } =
    do js1 <- run $ C.assertViolated ass
       js2 <- run $ C.assertNormal   ass
       Right (B.shortTrim js1, B.shortTrim js2)
    where
      run :: [C.ShortAssert c] -> B.Ab [B.OutputChunks c]
      run = mapM B.shortM . run2

      run2 :: [C.ShortAssert c] -> [B.Short (B.Ab [B.OutputChunk c])]
      run2 = B.shortMap $ C.runAssertJudges global

assembleRelmap :: forall c. B.AbMap (C.Section c)
assembleRelmap s@C.Section { C.secSlot   = gslot
                           , C.secTokmap = tokmaps
                           , C.secAssert = ass
                           , C.secCons   = C.RelmapCons
                                           { C.consLexmap = lexmap
                                           , C.consRelmap = relmap }} =
    do ass2 <- mapM assemble `B.shortMapM` ass
       Right $ s { C.secAssert = ass2 }
    where
      assemble :: B.AbMap (C.Assert c)
      assemble a =
          B.abortableFrom "assert" a $ do
            trees     <- C.slotTrees gslot [] $ C.assTree a
            (lx, lxs) <- lexmap gslot tokmaps [] trees
            --lxs     <- slotLexmap lexmap gslot tokmaps lx
            parts     <- B.sequenceSnd $ B.mapSndTo relmap lxs
            rmap      <- relmap lx
            Right $ a { C.assRelmap = Just rmap
                      , C.assParts  = parts }



-- ----------------------  Slot substitution

slotLexmap :: C.ConsLexmap -> [B.NamedTrees] -> [B.NamedTrees]
           -> C.Lexmap -> B.Ab [C.Rody C.Lexmap]
slotLexmap lexmap gslot tokmaps = loop where
    loop lx = let rop    = C.lexOpText  lx
                  rod    = C.lexOperand lx
                  subuse = loops $ C.lexSubmap lx
              in B.abortableFrom "slot" lx $
                 case lookup rop tokmaps of
                   Nothing    -> subuse
                   Just trees ->
                       do trees'   <- C.slotTrees gslot rod trees
                          (lx2, _) <- lexmap gslot tokmaps [] trees'
                          use2     <- loop lx2
                          use3     <- subuse
                          Right $ ((rop, rod), lx2) : use2 ++ use3

    loops :: [C.Lexmap] -> B.Ab [C.Rody C.Lexmap]
    loops [] = Right []
    loops (lx : lxs) = do lx'  <- loop lx
                          lxs' <- loops lxs
                          Right $ lx' ++ lxs'


{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Run section.
module Koshucode.Baala.Core.Section.Run
( runSection,
) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Lexmap          as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Section.Section as C

runSection :: (C.CContent c) => C.Global c -> [C.Section c] -> B.Ab (B.OutputResult c)
runSection global sects =
    do s2 <- assembleRelmap $ B.mconcat sects
       let js = C.secJudge s2
           g2 = global { C.globalJudges = js }
       case filter B.isViolative js of
         []  -> runSectionBody g2 s2
         jsV -> Right ([B.Short [] [] [B.OutputJudge jsV]], [])

runSectionBody :: forall c. (Ord c, B.Write c, C.CRel c, C.CEmpty c) =>
    C.Global c -> C.Section c -> B.Ab (B.OutputResult c)
runSectionBody global C.Section { C.secAssert = ass, C.secMessage = msg } =
    do js1 <- run $ C.assertViolated ass
       js2 <- run $ C.assertNormal   ass
       Right (B.shortTrim js1, msgChunk : B.shortTrim js2)
    where
      run :: [C.ShortAssert c] -> B.Ab [B.OutputChunks c]
      run = mapM $ C.runAssertJudges global

      msgChunk :: B.OutputChunks c
      msgChunk | null msg  = B.Short [] [] []
               | otherwise = B.Short [] [] [B.OutputComment message]

      message = "" : "MESSAGE" : map ("  " ++) msg ++ [""]

assembleRelmap :: forall c. B.AbMap (C.Section c)
assembleRelmap sec@C.Section { C.secSlot   = gslot
                             , C.secRelmap = tokmaps
                             , C.secAssert = ass
                             , C.secCons   = C.RelmapCons lexmap relmap } =
    do result <- B.shortListM $ mapM assemble `B.map2` ass
       let ass2 = map fst `B.map2` result
           msg  = map snd `B.map2` result
           msg2 = concat $ concat $ map B.shortBody msg
       Right $ C.addMessages msg2 $ sec { C.secAssert = ass2 }
    where
      assemble :: C.Assert c -> B.Ab (C.Assert c, [String])
      assemble a =
          B.abortable "assert" [a] $ do
            trees     <- C.substSlot gslot [] $ C.assTree a
            (lx, lxs) <- lexmap gslot tokmaps trees
            parts     <- B.sequenceSnd $ B.mapSndTo relmap lxs
            rmap      <- relmap lx
            let msg1  =  C.lexMessage lx
                msg2  =  concatMap C.lexMessageList $ map snd lxs
            Right ( a { C.assRelmap = Just rmap
                      , C.assParts  = parts }
                  , msg1 ++ msg2 )

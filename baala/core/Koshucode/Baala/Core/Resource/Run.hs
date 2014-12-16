{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Run resource.
module Koshucode.Baala.Core.Resource.Run
  ( runResource,
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Content           as C
import qualified Koshucode.Baala.Core.Lexmap            as C
import qualified Koshucode.Baala.Core.Relmap            as C
import qualified Koshucode.Baala.Core.Assert            as C
import qualified Koshucode.Baala.Core.Resource.Resource as C
import qualified Koshucode.Baala.Core.Message           as Msg

runResource :: (C.CContent c) => C.Global c -> C.Resource c -> B.Ab (B.OutputResult c)
runResource global res =
    do s2 <- assembleRelmap res
       let js = C.resJudge s2
           g2 = global { C.globalJudges = js }
       case filter B.isViolative js of
         []  -> runResourceBody g2 s2
         jsV -> Right ([B.Short [] [] [B.OutputJudge jsV]], [])

runResourceBody :: forall c. (Ord c, B.Write c, C.CRel c, C.CEmpty c) =>
    C.Global c -> C.Resource c -> B.Ab (B.OutputResult c)
runResourceBody global C.Resource { C.resAssert = ass, C.resMessage = msg } =
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

assembleRelmap :: forall c. B.AbMap (C.Resource c)
assembleRelmap res@C.Resource { C.resGlobal  = g
                              , C.resSlot    = slots
                              , C.resRelmap  = derives
                              , C.resAssert  = asserts } =
    do result <- B.shortListM $ mapM assemble `B.map2` asserts
       let asserts2  = map fst `B.map2` result
           msg       = map snd `B.map2` result
           msg2      = concat $ concat $ map B.shortBody msg
       Right $ C.addMessages msg2 $ res { C.resAssert = asserts2 }
    where
      (consLexmap, consRelmap) = C.relmapCons g

      assemble :: C.Assert c -> B.Ab (C.Assert c, [String])
      assemble ass =
          Msg.abAssert [ass] $ do
            trees      <- C.substSlot slots [] $ C.assTree ass
            (lx, lxs)  <- consLexmap slots derives trees
            relmap     <- consRelmap lx
            links      <- B.sequenceSnd $ B.mapSndTo consRelmap lxs
            let msg1    = C.lexMessage lx
                msg2    = concatMap C.lexMessageList $ map snd lxs
                ass2    = ass { C.assRelmap  = Just relmap
                              , C.assLinks   = links }
            Right (ass2, msg1 ++ msg2)

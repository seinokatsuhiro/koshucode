{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Run resource.
module Koshucode.Baala.Core.Resource.Run
  ( runResource,
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Core.Content            as C
import qualified Koshucode.Baala.Core.Lexmap             as C
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Assert             as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Core.Message            as Msg

runResource :: (C.CContent c) => C.Resource c -> B.Ab (B.OutputResult c)
runResource res =
    do s2 <- assembleRelmap res
       let js = C.resJudge s2
       case filter B.isViolative js of
         []  -> runResourceBody s2
         jsV -> Right ([B.Short [] [] [B.OutputJudge jsV]], [])

runResourceBody :: forall c. (Ord c, B.Write c, C.CRel c, C.CEmpty c) =>
    C.Resource c -> B.Ab (B.OutputResult c)
runResourceBody res@C.Resource { C.resAssert = ass, C.resMessage = msg } =
    do js1 <- run $ C.assertViolated ass
       js2 <- run $ C.assertNormal   ass
       Right (B.shortTrim js1, msgChunk : B.shortTrim js2)
    where
      run :: [C.ShortAssert c] -> B.Ab [B.OutputChunks c]
      run = mapM $ C.runAssertJudges res

      msgChunk :: B.OutputChunks c
      msgChunk | null msg  = B.Short [] [] []
               | otherwise = B.Short [] [] [B.OutputComment message]

      message = "" : "MESSAGE" : map ("  " ++) msg ++ [""]

assembleRelmap :: forall c. B.AbMap (C.Resource c)
assembleRelmap res@C.Resource { C.resSlot    = slots
                              , C.resRelmap  = derives
                              , C.resAssert  = asserts } = res'
    where
      res' = do result <- B.shortListM $ fmap assemble `map` asserts
                let asserts2  = fmap fst `map` result
                    msg       = fmap snd `map` result
                    msg2      = concat $ map B.shortBody msg
                Right $ C.addMessages msg2 $ res { C.resAssert = asserts2 }

      (consLexmap, consRelmap) = C.relmapCons res

      assemble :: C.Assert c -> B.Ab (C.Assert c, [String])
      assemble ass@C.Assert { C.assSection = sec } =
          Msg.abAssert [ass] $ do
            trees      <- C.substSlot slots [] $ C.assTree ass
            (lx, lxs)  <- consLexmap slots (findRelmap derives) sec trees
            relmap     <- consRelmap lx
            links      <- B.sequenceSnd $ B.mapSndTo consRelmap lxs
            let msg1    = C.lexMessage lx
                msg2    = concatMap C.lexMessageList $ map snd lxs
                ass2    = ass { C.assRelmap  = Just relmap
                              , C.assLinks   = links }
            Right (ass2, msg1 ++ msg2)

findRelmap :: [C.RelmapSource] -> C.FindRelmap
findRelmap ds sec name =
    case filter isSameName ds of
      [def]  -> [def]
      ds2    -> case filter isSameSec ds2 of
                  [def]  -> [def]
                  _      -> ds2
    where
      isSameName ((_, n), _)  = (n == name)
      isSameSec  ((s, _), _)  = (s == sec)

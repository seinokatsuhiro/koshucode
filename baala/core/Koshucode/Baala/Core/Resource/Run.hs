{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Run resource.
module Koshucode.Baala.Core.Resource.Run
  ( runResource,
    assembleRelmap,
    relmapCons,
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Core.Content            as C
import qualified Koshucode.Baala.Core.Lexmap             as C
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Assert             as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Core.Message            as Msg

runResource :: (C.CContent c) => C.Resource c -> B.Ab B.Result
runResource res =
    do res' <- assembleRelmap res
       let js = C.resJudge res'
       case filter B.isViolative js of
         []  -> runResourceBody res'
         jsV -> let jsV' = B.textualjudge B.shortEmpty `map` jsV
                in Right $ B.resultEmpty
                       { B.resultOutput   = C.resOutput res
                       , B.resultViolated = [B.Short [] [] [B.ResultJudge jsV']] }

runResourceBody :: forall c. (Ord c, B.Write c, C.CRel c, C.CEmpty c) =>
    C.Resource c -> B.Ab B.Result
runResourceBody res@C.Resource { C.resAssert  = ass
                               , C.resEcho    = echo
                               , C.resMessage = msg } =
    do js1 <- run $ C.assertViolated ass
       js2 <- run $ C.assertNormal   ass
       Right $ B.resultEmpty
                 { B.resultInput     = C.resInputPoint res
                 , B.resultOutput    = C.resOutput res
                 , B.resultEcho      = map B.lineContent `map` echo
                 , B.resultViolated  = B.shortTrim js1
                 , B.resultNormal    = msgChunk : B.shortTrim js2
                 , B.resultPattern   = C.resPattern res }
    where
      run :: [C.ShortAssert c] -> B.Ab [B.ResultChunks]
      run = let opt = C.resOption res
            in mapM (C.runAssertJudges res opt) . B.shortGroup

      msgChunk :: B.ResultChunks
      msgChunk | null msg  = B.Short [] [] []
               | otherwise = B.Short [] [] [B.ResultNote message]

      message = "" : "MESSAGE" : map ("  " ++) msg ++ [""]

assembleRelmap :: forall c. B.AbMap (C.Resource c)
assembleRelmap res@C.Resource { C.resSlot    = slots
                              , C.resLexmap  = derives
                              , C.resAssert  = asserts } = res'
    where
      res' = do result <- B.shortListM $ fmap assemble `map` asserts
                let asserts2  = fmap fst `map` result
                    msg       = fmap snd `map` result
                    msg2      = concat $ map B.shortBody msg
                Right $ C.addMessages msg2 $ res { C.resAssert = asserts2 }

      (consLexmap, consRelmap) = relmapCons res

      assemble :: C.Assert c -> B.Ab (C.Assert c, [String])
      assemble ass@C.Assert { C.assSection = sec } =
          Msg.abAssert [ass] $ do
            trees      <- C.substSlot slots [] $ B.paraPos $ C.assPara ass
            (lx, lxs)  <- consLexmap slots (findRelmap derives) sec trees
            relmap     <- consRelmap lx
            links      <- B.sequenceSnd $ B.mapSndTo consRelmap lxs
            let msg1    = C.lexMessage lx
                msg2    = concatMap C.lexMessageList $ map snd lxs
                ass2    = ass { C.assRelmap  = Just relmap
                              , C.assLinks   = links }
            Right (ass2, msg1 ++ msg2)

-- | Make a constructor pair of lexmap and relmap.
relmapCons :: (C.GetGlobal h) => h c -> (C.ConsLexmap, C.ConsRelmap' h c)
relmapCons hook = (consL, consR) where
    consL         = C.consLexmap findSorter
    consR         = C.consRelmap findRop hook
    findSorter n  = C.ropSorter `fmap` findRop n
    findRop       = C.opsetFindRop $ C.globalOpset $ C.getGlobal hook

findRelmap :: [C.LexmapClause] -> C.FindDeriv
findRelmap ds sec name =
    case filter isSameName ds of
      [def]  -> [def]
      ds2    -> case filter isSameSec ds2 of
                  [def]  -> [def]
                  _      -> ds2
    where
      isSameName ((_, n), _)  = (n == name)
      isSameSec  ((s, _), _)  = (s == sec)

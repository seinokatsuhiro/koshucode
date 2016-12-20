{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Run resource.
module Koshucode.Baala.Core.Resource.Run
  ( resRun,
    assembleRelmap,
    relmapCons,
  ) where

import qualified Data.Map.Strict                         as Ms
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Type                    as T
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core.Assert             as C
import qualified Koshucode.Baala.Core.Lexmap             as C
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Core.Assert.Message     as Msg

-- | Run resource.
resRun :: (D.CContent c) => C.Resource c -> B.Ab (C.Result c)
resRun res =
    do res' <- assembleRelmap $ autoOutputResource res
       let rslt = C.globalResult $ C.resGlobal res'
           js   = C.resJudge res'
       case filter T.isViolative js of
         []  -> resRunBody rslt res'
         jsV -> Right rslt
                  { C.resultOutput   = C.resOutput res
                  , C.resultViolated = [S.Short [] [] [C.ResultJudge jsV]] }

resRunBody :: forall c. (Ord c, D.CContent c) =>
    C.Result c -> C.Resource c -> B.Ab (C.Result c)
resRunBody rslt res@C.Resource { C.resAssert  = ass
                               , C.resEcho    = echo
                               , C.resLicense = license
                               , C.resMessage = msg } =
    do js1 <- run $ C.assertViolated ass
       js2 <- run $ C.assertNormal   ass
       Right rslt
               { C.resultInput     = C.resInputPoint res
               , C.resultOutput    = C.resOutput res
               , C.resultEcho      = map B.lineContent `map` echo
               , C.resultLicense   = group license
               , C.resultViolated  = S.shortTrim js1
               , C.resultNormal    = msgChunk : S.shortTrim js2
               , C.resultClass     = C.resClass res }
    where
      run :: [C.ShortAssert c] -> B.Ab [C.ShortResultChunks c]
      run = let opt = C.resOption res
            in mapM (C.runAssertJudges res opt) . S.shortGroup

      msgChunk :: C.ShortResultChunks c
      msgChunk | null msg  = S.Short [] [] []
               | otherwise = S.Short [] [] [C.ResultNote message]

      message = "" : "MESSAGE" : map ("  " ++) msg ++ [""]

      group :: [(C.SecNo, String)] -> [[String]]
      group = B.unique . map (reverse . snd) . B.gatherToAssoc

-- | Assemble relmap of assert clause.
assembleRelmap :: forall c. B.AbMap (C.Resource c)
assembleRelmap res@C.Resource { C.resSlot    = slots
                              , C.resLexmap  = derives
                              , C.resAssert  = asserts } = res'
    where
      res' = do result <- S.shortListM $ fmap assemble `map` asserts
                let asserts2  = fmap fst `map` result
                    msg       = fmap snd `map` result
                    msg2      = concat $ map S.shortBody msg
                Right $ C.resMessagesAdd msg2 $ res { C.resAssert = asserts2 }

      (consLexmap, consRelmap) = relmapCons res

      assemble :: C.Assert c -> B.Ab (C.Assert c, [String])
      assemble ass@C.Assert { C.assRelmap = Just _ } = Right (ass, [])
      assemble ass@C.Assert { C.assSection = sec } =
          Msg.abAssert [ass] $ do
            trees      <- S.substSlot slots [] $ S.paraPos $ C.assPara ass
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
    findSorter n  = C.ropParaze `fmap` findRop n
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


-- ---------------------- Automatic output

autoOutputResource :: (Ord c) => O.Map (C.Resource c)
autoOutputResource res@C.Resource { C.resDataset = ds
                                  , C.resAssert  = ass }
    | null ass && (C.featAutoOutput $ C.resFeature res)
                 = res { C.resAssert = a <$> Ms.assocs (D.datasetClasses ds) }
    | otherwise  = res
    where a (c, ts) = S.Short [] [] $ C.Assert
            { C.assSection = 0
            , C.assType    = T.AssertAffirm
            , C.assClass   = c
            , C.assToken   = []
            , C.assPara    = B.def
            , C.assRelmap  = Just $ C.RelmapSource B.def c ts
            , C.assLinks   = [] }


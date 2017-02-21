{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Koshucode output.

module Koshucode.Baala.Writer.Koshu
  ( resultKoshu,
    resultKoshu2,
    resultKoshuTab,
  ) where

import qualified System.IO                           as IO
import qualified Koshucode.Baala.Overture            as O
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Type                as T
import qualified Koshucode.Baala.Syntax              as S
import qualified Koshucode.Baala.Core                as C
import qualified Koshucode.Baala.Writer.Judge        as W

-- | Koshucode writer.
resultKoshu :: (B.MixEncode c) => C.ResultWriter c
resultKoshu = resultKoshu2

-- | Koshucode writer with two-space separator.
--   This function uses 'D.judgeMix2'.
resultKoshu2 :: (B.MixEncode c) => C.ResultWriter c
resultKoshu2 = C.ResultWriterChunk "koshu-2" $ hPutKoshu (T.judgeBreak, T.judgeMix2)

-- | Koshucode writer with tab separator.
--   This function uses 'D.judgeMixTab'.
resultKoshuTab :: (B.MixEncode c) => C.ResultWriter c
resultKoshuTab = C.ResultWriterChunk "koshu-tab" $ hPutKoshu (B.crlfBreak, T.judgeMixTab)

hPutKoshu :: (B.MixEncode c) => (B.LineBreak, T.EncodeJudge S.Chars c) -> C.ResultWriterChunk c
hPutKoshu output@(lb, _) h result _ sh =
    do -- head
       B.when (C.resultPrintHead result) $ hPutHead h result
       hPutLicense h result
       hPutEcho h result
       -- body
       let !gutter  = C.resultGutter result
           !measure = C.resultMeasure result
           !foot    = C.resultPrintFoot result
           !status  = C.resultStatus result
           !cnt     = W.judgeCount $ C.resultClass result
       cnt' <- B.hPutMixEither lb h cnt (mixShortChunk output gutter measure O.<++> sh)
       -- foot
       B.when foot $ hPutFoot h status cnt'

hPutHead :: IO.Handle -> C.Result c -> IO ()
hPutHead h result =
    do IO.hPutStrLn h B.emacsModeComment
       O.hPutLines  h $ B.texts $ comm inputs
       O.hPutLn     h
    where
      inputs = C.inputPoint  `map` C.resultInput result
      itext  = B.ioPointText `map` inputs
      otext  = B.ioPointText $ C.resultOutput result

      comm _ | itext == [otext]
             = B.CommentDoc [ B.CommentSec "INPUT / OUTPUT" itext ]
      comm [B.IOPointCustom _ _]
             = B.CommentDoc [ B.CommentSec "INPUT / OUTPUT" itext ]
      comm _ = B.CommentDoc [ B.CommentSec "INPUT"  itext
                            , B.CommentSec "OUTPUT" [otext] ]

hPutLicense :: IO.Handle -> C.Result c -> IO ()
hPutLicense h C.Result { C.resultLicense = ls }
    | null ls    = return ()
    | otherwise  = do mapM_ put ls
                      IO.hPutStrLn h "=== rel"
                      O.hPutLn     h
    where
      put license =
          do IO.hPutStrLn h "=== license"
             O.hPutLn     h
             O.hPutLines  h license
             O.hPutLn     h

hPutEcho :: IO.Handle -> C.Result c -> IO ()
hPutEcho h result =
    do let echo = C.resultEcho result
       O.hPutLines h (O.tString <$> concat echo)
       B.when (echo /= []) $ O.hPutLn h

hPutFoot :: IO.Handle -> B.ExitCode -> W.JudgeCount -> IO ()
hPutFoot h status cnt = B.hPutMix B.crlfBreak h $ W.judgeSummary status cnt


-- ----------------------  Chunk

mixShortChunk
    :: (B.MixEncode c) => (B.LineBreak, T.EncodeJudge S.Chars c) -> Int -> Int
    -> C.ShortResultChunks c -> [W.JudgeCount -> [B.MixEither W.JudgeCount]]
mixShortChunk output gutter measure (S.Short _ def chunks) = ms where
    ms = short : mixChunks output gutter measure (S.shortText (t <$> def)) chunks
    short cnt = mixShort def cnt
    t (a, b) = (O.stringT a, O.stringT b)

mixShort :: [S.ShortDef String] -> W.JudgeCount -> [B.MixEither W.JudgeCount]
mixShort [] cnt = [Left cnt]
mixShort def cnt = Left cnt : (ms O.++ [Right B.mixHard]) where
    ms = (Right . B.mixLine) <$> (B.mix "short" : map shortLine def)
    shortLine :: (String, String) -> B.MixText
    shortLine (a, b) = B.mix "  " O.++ B.mix (O.padEnd width a) O.++
                       B.mix " "  O.++ B.mixShow b
    width :: Int
    width = maximum $ map (length . fst) def

{-| Output result chunk. -}
mixChunks
    :: (B.MixEncode c)
    => (B.LineBreak, T.EncodeJudge S.Chars c) -> Int -> Int -> B.TransText S.Chars
    -> [C.ResultChunk c] -> [W.JudgeCount -> [B.MixEither W.JudgeCount]]
mixChunks (_, encode) gutter measure sh = (rights <$>) where
    rights (C.ResultJudge js) (_, tab) =
        W.mixJudgesCount gutter measure (encode sh) js (0, tab)
    rights (C.ResultNote []) cnt = [Left cnt]
    rights (C.ResultNote ls) cnt = mixNote ls cnt
    rights (C.ResultRel _ _) cnt = [Left cnt]

mixNote :: [String] -> W.JudgeCount -> [B.MixEither W.JudgeCount]
mixNote ls cnt = Left cnt : notes where
    notes = Right <$> [ B.mixLine $ B.mix "=== note"
                      , B.mixHard
                      , B.mixLines (B.mix <$> ls)
                      , B.mixHard
                      , B.mixLine $ B.mix "=== rel"
                      , B.mixHard ]

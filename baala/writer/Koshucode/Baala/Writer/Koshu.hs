{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Koshucode output.

module Koshucode.Baala.Writer.Koshu
  ( resultKoshu,
    resultKoshu2,
    resultKoshuTab,
  ) where

import qualified Control.Monad                       as M
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

hPutKoshu :: (B.MixEncode c) => (B.LineBreak, T.EncodeJudge c) -> C.ResultWriterChunk c
hPutKoshu output h result status sh =
    do -- head
       B.when (C.resultPrintHead result) $ hPutHead h result
       hPutLicense h result
       hPutEcho h result
       -- body
       let cnt = W.judgeCount $ C.resultClass result
       cnt' <- M.foldM (hPutShortChunk output h result) cnt sh
       -- foot
       B.when (C.resultPrintFoot result) $ hPutFoot h status cnt'
       return status

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
       O.hPutLines h $ concat echo
       B.when (echo /= []) $ O.hPutLn h

hPutFoot :: IO.Handle -> B.ExitCode -> W.JudgeCount -> IO ()
hPutFoot h status cnt = O.hPutLines h $ W.judgeSummary status cnt


-- ----------------------  Chunk

hPutShortChunk
    :: (B.MixEncode c) => (B.LineBreak, T.EncodeJudge c) -> IO.Handle -> C.Result c
    -> W.JudgeCount -> C.ShortResultChunks c -> IO W.JudgeCount
hPutShortChunk output h result cnt (S.Short _ def chunks) =
    do hPutShort h def
       hPutChunks output h result (S.shortText def) chunks cnt

hPutShort :: IO.Handle -> [S.ShortDef] -> IO ()
hPutShort _ [] = return ()
hPutShort h def =
    do O.hPutLines h $ "short" : map shortLine def
       O.hPutLn    h
    where
      shortLine :: (String, String) -> String
      shortLine (a, b) = "  " ++ O.padEnd width a ++
                         " "  ++ show b
      width :: Int
      width = maximum $ map (length . fst) def

-- | Output result chunk.
hPutChunks
    :: (B.MixEncode c)
    => (B.LineBreak, T.EncodeJudge c) -> IO.Handle -> C.Result c -> B.TransString 
    -> [C.ResultChunk c] -> W.JudgeCount -> IO W.JudgeCount
hPutChunks (lb, encode) h result sh = loop where
    loop [] cnt                            = return cnt
    loop (C.ResultJudge js : xs) (_, tab)  =
        case W.judgesCountMix result (encode sh) js (B.mixEmpty, 0, tab) of
          (mx, cnt', tab') -> do B.hPutMix lb h mx
                                 loop xs (cnt', tab')
    loop (C.ResultNote [] : xs) cnt        = loop xs cnt
    loop (C.ResultNote ls : xs) cnt        = do hPutNote h ls
                                                loop xs cnt
    loop (C.ResultRel _ _ : xs) cnt        = loop xs cnt

hPutNote :: IO.Handle -> [String] -> IO ()
hPutNote h ls =
    do IO.hPutStrLn h "=== note"
       O.hPutLn     h
       O.hPutLines  h ls
       O.hPutLn     h
       IO.hPutStrLn h "=== rel"
       O.hPutLn     h


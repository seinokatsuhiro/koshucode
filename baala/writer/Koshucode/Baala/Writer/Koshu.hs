{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Writer.Koshu
  ( resultKoshu,
  ) where

import qualified Control.Monad                       as M
import qualified System.IO                           as IO
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Data                as D
import qualified Koshucode.Baala.Core                as C
import qualified Koshucode.Baala.Writer.Judge        as W

resultKoshu :: (B.Write c) => C.ResultWriter c
resultKoshu = C.ResultWriterChunk "koshu" hPutKoshu

hPutKoshu :: (B.Write c) => C.ResultWriterChunk c
hPutKoshu h result status sh =
    do -- head
       B.when (C.resultPrintHead result) $ hPutHead h result
       hPutLicense h result
       hPutEcho h result
       -- body
       let cnt = W.judgeCount $ C.resultPattern result
       cnt' <- M.foldM (hPutShortChunk h result) cnt sh
       -- foot
       B.when (C.resultPrintFoot result) $ hPutFoot h status cnt'
       return status

hPutHead :: IO.Handle -> C.Result c -> IO ()
hPutHead h result =
    do IO.hPutStrLn h B.emacsModeComment
       B.hPutLines  h $ B.texts $ comm inputs
       B.hPutEmptyLine h
    where
      inputs = C.inputPoint  `map` C.resultInput result
      itext  = B.ioPointText `map` inputs
      otext  = B.ioPointText $ C.resultOutput result

      comm [B.IOPointCustom _ _]
             = B.CommentDoc [ B.CommentSec "INPUT / OUTPUT" itext ]
      comm _ = B.CommentDoc [ B.CommentSec "INPUT"  itext
                            , B.CommentSec "OUTPUT" [otext] ]

hPutLicense :: IO.Handle -> C.Result c -> IO ()
hPutLicense h C.Result { C.resultLicense = ls }
    | null ls    = return ()
    | otherwise  = do mapM_ put ls
                      IO.hPutStrLn h "=== rel"
                      B.hPutEmptyLine h
    where
      put license =
          do IO.hPutStrLn h "=== license"
             B.hPutEmptyLine h
             B.hPutLines h license
             B.hPutEmptyLine h

hPutEcho :: IO.Handle -> C.Result c -> IO ()
hPutEcho h result =
    do let echo = C.resultEcho result
       B.hPutLines h $ concat echo
       B.when (echo /= []) $ B.hPutEmptyLine h

hPutFoot :: IO.Handle -> B.ExitCode -> W.JudgeCount -> IO ()
hPutFoot h status cnt = B.hPutLines h $ W.judgeSummary status cnt


-- ----------------------  Chunk

hPutShortChunk :: (B.Write c) => IO.Handle -> C.Result c -> W.JudgeCount -> C.ShortResultChunks c -> IO W.JudgeCount
hPutShortChunk h result cnt (D.Short _ def output) =
    do hPutShort h def
       hPutChunks h result (D.shortText def) output cnt

hPutShort :: IO.Handle -> [D.ShortDef] -> IO ()
hPutShort _ [] = return ()
hPutShort h def =
    do B.hPutLines h $ "short" : map shortLine def
       B.hPutEmptyLine h
    where
      shortLine :: (String, String) -> String
      shortLine (a, b) = "  " ++ B.padRight width a ++
                         " "  ++ show b
      width :: Int
      width = maximum $ map (length . fst) def

hPutChunks :: (B.Write c) => IO.Handle -> C.Result c -> B.StringMap -> [C.ResultChunk c] -> W.JudgeCount -> IO W.JudgeCount
hPutChunks h result sh = loop where
    writer = IO.hPutStrLn h . D.writeDownJudge sh

    loop [] cnt                            = return cnt
    loop (C.ResultJudge js : xs) (_, tab)  = do cnt' <- W.hPutJudgesCount h result writer js (0, tab)
                                                loop xs cnt'
    loop (C.ResultNote [] : xs) cnt        = loop xs cnt
    loop (C.ResultNote ls : xs) cnt        = do hPutNote h ls
                                                loop xs cnt
    loop (C.ResultRel _ _ : xs) cnt        = loop xs cnt

hPutNote :: IO.Handle -> [String] -> IO ()
hPutNote h ls =
    do IO.hPutStrLn    h "=== note"
       B.hPutEmptyLine h
       B.hPutLines     h ls
       B.hPutEmptyLine h
       IO.hPutStrLn    h "=== rel"
       B.hPutEmptyLine h


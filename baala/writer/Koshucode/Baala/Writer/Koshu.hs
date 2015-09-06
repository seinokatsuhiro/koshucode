{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Writer.Koshu
  ( resultKoshu,
  ) where

import qualified Control.Monad                       as M
import qualified System.IO                           as IO
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Writer.Judge        as B

resultKoshu :: (B.Write c) => B.ResultWriter c
resultKoshu = B.ResultWriterChunk "koshu" hPutKoshu

hPutKoshu :: (B.Write c) => B.ResultWriterChunk c
hPutKoshu h result status sh =
    do -- head
       B.when (B.resultPrintHead result) $ hPutHead h result
       hPutLicense h result
       hPutEcho h result
       -- body
       let cnt = B.judgeCount $ B.resultPattern result
       cnt' <- M.foldM (hPutShortChunk h result) cnt sh
       -- foot
       B.when (B.resultPrintFoot result) $ hPutFoot h status cnt'
       return status

hPutHead :: IO.Handle -> B.Result c -> IO ()
hPutHead h result =
    do IO.hPutStrLn h B.emacsModeComment
       B.hPutLines  h $ B.texts comm
       B.hPutEmptyLine h
    where
      itext = (B.ioPointText . B.inputPoint) `map` B.resultInput result
      otext = B.ioPointText $ B.resultOutput result
      comm  = B.CommentDoc [ B.CommentSec "INPUT"  itext
                           , B.CommentSec "OUTPUT" [otext] ]

hPutLicense :: IO.Handle -> B.Result c -> IO ()
hPutLicense h B.Result { B.resultLicense = ls }
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

hPutEcho :: IO.Handle -> B.Result c -> IO ()
hPutEcho h result =
    do let echo = B.resultEcho result
       B.hPutLines h $ concat echo
       B.when (echo /= []) $ B.hPutEmptyLine h

hPutFoot :: IO.Handle -> B.ExitCode -> B.JudgeCount -> IO ()
hPutFoot h status cnt = B.hPutLines h $ B.judgeSummary status cnt


-- ----------------------  Chunk

hPutShortChunk :: (B.Write c) => IO.Handle -> B.Result c -> B.JudgeCount -> B.ShortResultChunks c -> IO B.JudgeCount
hPutShortChunk h result cnt (B.Short _ def output) =
    do hPutShort h def
       hPutChunks h result (B.shortText def) output cnt

hPutShort :: IO.Handle -> [B.ShortDef] -> IO ()
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

hPutChunks :: (B.Write c) => IO.Handle -> B.Result c -> B.StringMap -> [B.ResultChunk c] -> B.JudgeCount -> IO B.JudgeCount
hPutChunks h result sh = loop where
    writer = IO.hPutStrLn h . B.writeDownJudge sh

    loop [] cnt                            = return cnt
    loop (B.ResultJudge js : xs) (_, tab)  = do cnt' <- B.hPutJudgesCount h result writer js (0, tab)
                                                loop xs cnt'
    loop (B.ResultNote [] : xs) cnt        = loop xs cnt
    loop (B.ResultNote ls : xs) cnt        = do hPutNote h ls
                                                loop xs cnt
    loop (B.ResultRel _ _ : xs) cnt        = loop xs cnt

hPutNote :: IO.Handle -> [String] -> IO ()
hPutNote h ls =
    do IO.hPutStrLn    h "=== note"
       B.hPutEmptyLine h
       B.hPutLines     h ls
       B.hPutEmptyLine h
       IO.hPutStrLn    h "=== rel"
       B.hPutEmptyLine h


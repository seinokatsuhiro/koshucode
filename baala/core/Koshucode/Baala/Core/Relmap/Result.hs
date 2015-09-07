{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Result of relational calculation.

module Koshucode.Baala.Core.Relmap.Result
  ( -- * Result
    Result (..),
    InputPoint (..),
    resultEmpty,

    -- * Chunk
    ResultChunk (..),
    ResultWriterChunk, ResultWriterJudge,
    ShortResultChunks,
    resultChunkJudges,
    resultDump,
  
    -- * Writer
    ResultWriter (..),
    putResult, hPutResult,
    useUtf8,
  ) where

import qualified GHC.IO.Encoding                   as IO
import qualified System.IO                         as IO
import qualified Koshucode.Baala.Base              as B


-- ----------------------  Result

-- | Result of calculation.
data Result c = Result
    { resultWriter     :: ResultWriter c
    , resultPrintHead  :: Bool
    , resultPrintFoot  :: Bool
    , resultGutter     :: Int
    , resultMeasure    :: Int
    , resultInput      :: [InputPoint]
    , resultOutput     :: B.IOPoint
    , resultEcho       :: [[String]]
    , resultLicense    :: [[String]]
    , resultViolated   :: [ShortResultChunks c]
    , resultNormal     :: [ShortResultChunks c]
    , resultPattern    :: [B.JudgePat]
    } deriving (Show, Eq, Ord)

data InputPoint = InputPoint
    { inputPoint      :: B.IOPoint
    , inputPointAbout :: [B.TTree]
    } deriving (Show, Eq, Ord)

-- | Empty result.
resultEmpty :: (Show c, B.Write c) => Result c
resultEmpty =
    Result { resultWriter     = resultDump
           , resultPrintHead  = True
           , resultPrintFoot  = True
           , resultGutter     = 5
           , resultMeasure    = 25
           , resultInput      = []
           , resultOutput     = B.IOPointStdout
           , resultEcho       = []
           , resultLicense    = []
           , resultViolated   = []
           , resultNormal     = []
           , resultPattern    = [] }


-- ----------------------  Chunk

-- | Chunk of judgements.
data ResultChunk c
    = ResultJudge  [B.Judge c]
    | ResultRel    B.JudgePat (B.Rel c)
    | ResultNote   [String]
      deriving (Show, Eq, Ord)

type ResultWriterChunk c = IO.Handle -> Result c -> B.ExitCode -> [ShortResultChunks c] -> IO B.ExitCode
type ResultWriterJudge c = IO.Handle -> Result c -> B.ExitCode -> [B.Judge c] -> IO B.ExitCode

type ShortResultChunks c = B.Short [ResultChunk c]

resultChunkJudges :: ResultChunk c -> [B.Judge c]
resultChunkJudges (ResultJudge js) = js
resultChunkJudges _ = []

resultDump :: (Show c) => ResultWriter c
resultDump = ResultWriterChunk "show" hPutShow

hPutShow :: (Show c) => ResultWriterChunk c
hPutShow h result status _ = do IO.hPutStrLn h $ show result
                                return status


-- ----------------------  Writer

data ResultWriter c
    = ResultWriterChunk String (ResultWriterChunk c)
    | ResultWriterJudge String (ResultWriterJudge c)

instance Show (ResultWriter c) where
    show (ResultWriterChunk n _) = "ResultWriterChunk " ++ n
    show (ResultWriterJudge n _) = "ResultWriterJudge " ++ n

instance Ord (ResultWriter c) where
    compare w1 w2 = B.name w1 `compare` B.name w2

instance Eq (ResultWriter c) where
    a == b  = compare a b == EQ

instance B.Name (ResultWriter c) where
    name (ResultWriterChunk n _) = n
    name (ResultWriterJudge n _) = n

-- | `B.stdout` version of `hPutResult`.
putResult :: (B.Write c) => Result c -> IO B.ExitCode
putResult result =
    case resultOutput result of
      B.IOPointStdout      -> hPutResult B.stdout result
      B.IOPointFile _ path -> do h <- IO.openFile path IO.WriteMode
                                 n <- hPutResult h result
                                 IO.hClose h
                                 return n
      output -> B.bug $ "putResult " ++ show output

-- | Print result of calculation, and return status.
hPutResult :: forall c. (B.Write c) => IO.Handle -> Result c -> IO B.ExitCode
hPutResult h result
    | null violated  = hPutAllChunks h result (B.exitCode 0) normal
    | otherwise      = hPutAllChunks h result (B.exitCode 1) violated
    where
      normal, violated :: [ShortResultChunks c]
      normal    = resultNormal result
      violated  = B.shortTrim $ B.map2 (filter hasJudge) $ resultViolated result

      hasJudge :: ResultChunk c -> Bool
      hasJudge (ResultJudge js)  = B.notNull js
      hasJudge _                 = False

hPutAllChunks :: (B.Write c) => ResultWriterChunk c
hPutAllChunks h result status sh =
    do useUtf8 h
       case resultWriter result of
         ResultWriterChunk _ w -> w h result status sh
         ResultWriterJudge _ w -> w h result status $ judges sh
    where
      judges :: [ShortResultChunks c] -> [B.Judge c]
      judges = concatMap resultChunkJudges . concatMap B.shortBody

useUtf8 :: IO.Handle -> IO ()
useUtf8 h = do IO.setLocaleEncoding IO.utf8_bom
               IO.hSetEncoding h IO.utf8


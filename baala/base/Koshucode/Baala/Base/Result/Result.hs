{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Result of relational calculation.

module Koshucode.Baala.Base.Result.Result
  ( -- * Result
    Result (..),
    InputPoint (..),
    resultEmpty,

    -- * Chunk
    ResultChunk (..),
    ResultWriterChunk, ResultWriterJudge,
    ShortResultChunks,
    resultChunkJudges,
    resultShow,
  
    -- * Writer
    ResultWriter (..),
    putResult, hPutResult,
    useUtf8,
  ) where

import qualified GHC.IO.Encoding                   as IO
import qualified System.IO                         as IO
import qualified Koshucode.Baala.Base.Abort        as B
import qualified Koshucode.Baala.Base.Data         as B
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B


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
    Result { resultWriter     = resultShow
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

type ResultWriterChunk c = Result c -> IO.Handle -> Int -> [ShortResultChunks c] -> IO Int
type ResultWriterJudge c = IO.Handle -> Result c -> Int -> [B.Judge c] -> IO Int

type ShortResultChunks c = B.Short [ResultChunk c]

resultChunkJudges :: ResultChunk c -> [B.Judge c]
resultChunkJudges (ResultJudge js) = js
resultChunkJudges _ = []

resultShow :: (Show c) => ResultWriter c
resultShow = ResultWriterChunk "show" hPutShow

hPutShow :: (Show c) => ResultWriterChunk c
hPutShow result h status _ = do IO.hPutStrLn h $ show result
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
putResult :: (B.Write c) => Result c -> IO Int
putResult result =
    case resultOutput result of
      B.IOPointStdout      -> hPutResult B.stdout result
      B.IOPointFile _ path -> do h <- IO.openFile path IO.WriteMode
                                 n <- hPutResult h result
                                 IO.hClose h
                                 return n
      output -> B.bug $ "putResult " ++ show output

-- | Print result of calculation, and return status.
hPutResult :: forall c. (B.Write c) => IO.Handle -> Result c -> IO Int
hPutResult h result
    | null violated  = hPutAllChunks result h 0 normal
    | otherwise      = hPutAllChunks result h 1 violated
    where
      normal, violated :: [ShortResultChunks c]
      normal    = resultNormal result
      violated  = B.shortTrim $ B.map2 (filter hasJudge) $ resultViolated result

      hasJudge :: ResultChunk c -> Bool
      hasJudge (ResultJudge js)  = B.notNull js
      hasJudge _                 = False

hPutAllChunks :: (B.Write c) => ResultWriterChunk c
hPutAllChunks result h status sh =
    do useUtf8 h
       case resultWriter result of
         ResultWriterChunk _ w -> w result h status sh
         ResultWriterJudge _ w -> w h result status $ judges sh
    where
      judges :: [ShortResultChunks c] -> [B.Judge c]
      judges = concatMap resultChunkJudges . concatMap B.shortBody

useUtf8 :: IO.Handle -> IO ()
useUtf8 h = do IO.setLocaleEncoding IO.utf8_bom
               IO.hSetEncoding h IO.utf8



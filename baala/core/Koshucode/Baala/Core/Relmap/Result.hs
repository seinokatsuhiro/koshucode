{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Result of relational calculation.

module Koshucode.Baala.Core.Relmap.Result
  ( -- * Result
    Result (..),
    ResultPortable (..),
    InputPoint (..),

    -- * Chunk
    ShortResultChunks,
    ResultChunk (..),
    ResultOption (..),
    resultChunkJudges,
  
    -- * Writer
    ResultWriter (..),
    ResultWriterFrom,
    ResultWriterRaw,
    ResultWriterChunk,
    ResultWriterJudge,
    resultDump,
    putResult,
  ) where

import qualified System.IO                         as IO
import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.System            as O
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S
import qualified Koshucode.Baala.Type              as T
import qualified Koshucode.Baala.Data              as D


-- ----------------------  Result

{-| Result of calculation. -}
data Result c = Result
    { resultWriter     :: ResultWriter c        -- ^ Writer
    , resultPortable   :: ResultPortable        -- ^ Portable property
    , resultInput      :: [InputPoint S.Chars]  -- ^ Input points
    , resultOutput     :: B.IOPoint             -- ^ Output point
    , resultEcho       :: [[S.Chars]]           -- ^ Echo messages
    , resultLicense    :: [[String]]            -- ^ License texts
    , resultViolated   :: [ShortResultChunks c] -- ^ Vailated output
    , resultNormal     :: [ShortResultChunks c] -- ^ Normal output
    , resultClass      :: [S.JudgeClass]        -- ^ List of judgement classes
    } deriving (Show, Eq, Ord)

{-| Portable properties of calculation result. -}
data ResultPortable = ResultPortable
    { resultPrintHead  :: Bool                  -- ^ Print header comment
    , resultPrintFoot  :: Bool                  -- ^ Print fotter comment
    , resultGutter     :: Int                   -- ^ Interval between gutters
    , resultMeasure    :: Int                   -- ^ Interval between measure
    , resultStatus     :: B.ExitCode            -- ^ Status code
    } deriving (Show, Eq, Ord)

-- | Input point of data resource.
data InputPoint t = InputPoint
    { inputPoint      :: B.IOPoint    -- ^ Input point
    , inputPointAbout :: [S.TTree t]  -- ^ Common terms
    } deriving (Show, Eq, Ord)

-- | Empty result.
instance (Show c) => B.Default (Result c) where
    def = Result { resultWriter     = resultDump
                 , resultPortable   = ResultPortable
                                      { resultPrintHead  = True
                                      , resultPrintFoot  = True
                                      , resultGutter     = 5
                                      , resultMeasure    = 25
                                      , resultStatus     = O.exitCode 0
                                      }
                 , resultInput      = []
                 , resultOutput     = B.IOPointStdout Nothing
                 , resultEcho       = []
                 , resultLicense    = []
                 , resultViolated   = []
                 , resultNormal     = []
                 , resultClass      = [] }


-- ----------------------  Chunk

{-| Short block of result chunks. -}
type ShortResultChunks c = S.Short String [ResultChunk c]

{-| Chunk of judgements. -}
data ResultChunk c
    = ResultChunk  T.AssertType S.JudgeClass
                   (T.Rel c) ResultOption  -- ^ General chunk
    | ResultJudge  [T.Judge c]             -- ^ List of judges
    | ResultNote   [String]                -- ^ Commentary note
      deriving (Show, Eq, Ord)

{-| Options of result data. -}
data ResultOption = ResultOption {
      resultShowEmpty :: Bool         -- ^ Show empty term
    } deriving (Show, Eq, Ord)

{-| Extract judgement list from result. -}
resultChunkJudges :: (D.CEmpty c) => ResultChunk c -> [T.Judge c]
resultChunkJudges (ResultChunk ty cls r ResultOption { resultShowEmpty = e })
    = T.judgesFromRel (assert e ty) cls r
resultChunkJudges (ResultJudge js) = js
resultChunkJudges _ = []

assert :: (D.CEmpty c) => Bool -> T.AssertType -> T.JudgeOf c
assert True  ty cls = T.assertAs ty cls
assert False ty cls = T.assertAs ty cls . D.cutEmpty


-- ----------------------  Writer

-- | Writer of calculation result.
data ResultWriter c
    = ResultWriterRaw    String (ResultWriterRaw c)    -- ^ Write from result
    | ResultWriterChunk  String (ResultWriterChunk c)  -- ^ Write from short chunks
    | ResultWriterJudge  String (ResultWriterJudge c)  -- ^ Write from judges

instance Show (ResultWriter c) where
    show w = S.subtypeName w ++ " " ++ B.name w

instance S.SubtypeName (ResultWriter c) where
    subtypeName (ResultWriterRaw   _ _) = "ResultWriterRaw"
    subtypeName (ResultWriterChunk _ _) = "ResultWriterChunk"
    subtypeName (ResultWriterJudge _ _) = "ResultWriterJudge"

instance Ord (ResultWriter c) where
    compare w1 w2 = B.name w1 `compare` B.name w2

instance Eq (ResultWriter c) where
    a == b  = compare a b == EQ

instance B.Name (ResultWriter c) where
    name (ResultWriterRaw   n _) = n
    name (ResultWriterChunk n _) = n
    name (ResultWriterJudge n _) = n

-- | Type of result writer function.
type ResultWriterFrom d c = IO.Handle -> Result c -> d -> IO ()

-- | Write result based on result itself.
type ResultWriterRaw c = ResultWriterFrom () c

-- | Write result based on its short chunks.
type ResultWriterChunk c = ResultWriterFrom [ShortResultChunks c] c

-- | Write result based on its judges.
type ResultWriterJudge c = ResultWriterFrom [T.Judge c] c

-- | Dump result.
resultDump :: (Show c) => ResultWriter c
resultDump = ResultWriterRaw "show" hPutShow

hPutShow :: (Show c) => ResultWriterRaw c
hPutShow h result () = IO.hPutStrLn h $ show result

-- | Print calculation result.
putResult :: (D.CEmpty c) => Result c -> IO B.ExitCode
putResult result =
    case resultOutput result of
      B.IOPointStdout _    -> hPutResult B.stdout result
      B.IOPointOutput h    -> hPutResult (B.handle h) result
      B.IOPointFile _ path -> do h <- IO.openFile path IO.WriteMode
                                 n <- hPutResult h result
                                 IO.hClose h
                                 return n
      output -> B.bug $ "putResult " ++ show output

-- | Print result of calculation, and return status.
hPutResult :: (D.CEmpty c) => IO.Handle -> Result c -> IO B.ExitCode
hPutResult h result =
    case resultShortChunks result of
      Right sh  -> do let !status = resultStatus $ resultPortable result
                      hPutAllChunks h result sh
                      return status
      Left  sh  -> do let status  = O.exitCode 1
                          result' = result { resultPortable =
                                                 let non = resultPortable result
                                                 in non { resultStatus = status }}
                      hPutAllChunks h result' sh
                      return status

resultShortChunks :: Result c -> O.Eith [ShortResultChunks c]
resultShortChunks Result {..}
    | null violated  = Right resultNormal
    | otherwise      = Left  violated
    where
      violated  = S.shortTrim (filter hasData O.<$$> resultViolated)

hasData :: ResultChunk c -> Bool
hasData (ResultChunk _ _ (T.Rel _ bo) _) = O.some bo
hasData (ResultJudge js)  = O.some js
hasData _                 = False

hPutAllChunks :: (D.CEmpty c) => ResultWriterChunk c
hPutAllChunks h result sh =
    do B.hSetKoshuOutput h
       case resultWriter result of
         ResultWriterRaw   _ w  -> w h result ()
         ResultWriterChunk _ w  -> w h result sh
         ResultWriterJudge _ w  -> w h result $ shortChunkJudges sh

shortChunkJudges :: (D.CEmpty c) => [ShortResultChunks c] -> [T.Judge c]
shortChunkJudges = concatMap resultChunkJudges . concatMap S.shortBody


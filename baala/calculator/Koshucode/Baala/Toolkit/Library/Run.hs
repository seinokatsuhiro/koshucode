{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Toolkit.Library.Run
( SectionSource (..)
, runFiles
, hRunFiles
, concatMM
, runCalc
, runCalcTo
, theContent
, readSec
, mkdir
) where

import Data.Monoid
import System.IO
import qualified System.FilePath as Path
import qualified System.Directory as Dir

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Toolkit.Library.Comment
import qualified Koshucode.Baala.Base.Prelude  as Kit
import qualified Koshucode.Baala.Base.Section  as Kit
import qualified Koshucode.Baala.Base.Data     as Kit


data SectionSource c = SectionSource
    { rootSection  :: Kit.Section c
    , textSections :: [String]
    , fileSections :: [FilePath]
    } deriving (Show)
      

-- ----------------------

runFiles :: (Value v) => SectionSource v -> IO ()
runFiles = hRunFiles stdout

{-| Read and union sections from files, and run the section. -}
hRunFiles
    :: (Value v)
    => Handle          -- ^ File handle
    -> SectionSource v -- ^ Section source code
    -> IO ()
hRunFiles h (SectionSource root textSec files) =
    do let sec = map (Kit.sectionRead root) $ textSec
       sects <- mapM (Kit.sectionFile root) $ files
       let union = concatMM $ sec ++ sects
           comm  = CommentDoc
                   [ CommentSec "INPUT" files]
       hSetEncoding h utf8
       hPutStrLn    h emacsModeComment
       hPutStr      h $ unlines $ texts comm
       hPutStrLn    h ""
       abortIO (Kit.hRunSectionIO h) union

concatMM :: (Monad m, Monoid a) => [m a] -> m a
concatMM [] = return mempty
concatMM (s:ss) = do s'  <- s
                     ss' <- concatMM ss
                     return $ mappend s' ss'



-- ---------------------- Calculation list

runCalc :: (Value v) => SectionSource v -> IO ()
runCalc = runCalcTo ""

runCalcTo
    :: (Value v)
    => FilePath          -- ^ Output path prefix
    -> SectionSource v   -- ^ Section
    -> IO ()             -- ^
runCalcTo dir sec =
    do union <- readSec sec
       abortIO (runCalcSec dir $ rootSection sec) union

runCalcSec :: (Value v) => String -> Kit.Section v -> Kit.Section v -> IO ()
runCalcSec dir root sec =
    do let js = Kit.sectionJudge sec
       mapM_ (runCalcJudge dir root) js
       return ()

runCalcJudge :: (Value v) => String -> Kit.Section v -> Judge v -> IO ()
runCalcJudge dir root (Judge True "KOSHU-CALC" xs) =
    case theContents ["/input", "/output"] xs of
      Just [input, output] ->
          do let inputFiles = theStrings input
                 outputFile = dir ++ Kit.theStringValue output
             putStrLn $ "**  Output to " ++ outputFile
             mkdir outputFile
             withFile outputFile WriteMode
                          $ \ h -> hRunFiles h
                                   (SectionSource root [] inputFiles)
      Just _       -> return ()
      Nothing      -> return ()
runCalcJudge _ _ _ =  return ()

mkdir :: FilePath -> IO ()
mkdir path = 
    do let dir = Path.dropFileName path
       Dir.createDirectoryIfMissing True dir



-- ----------------------  The

theContent :: (Value c) => String -> [Kit.Named c] -> Maybe c
theContent = lookup

theContents :: (Value c) => [String] -> [Kit.Named c] -> Maybe [c]
theContents ns termset = mapM (`theContent` termset) ns

theStrings :: (Value c) => c -> [String]
theStrings c | Kit.isStringValue c = [Kit.theStringValue c]
theStrings c | Kit.isListValue c   = map Kit.theStringValue
                                     $ Kit.theListValue c
theStrings _ = []



-- ----------------------  Read

readSec
    :: (Value v)
    => SectionSource v               -- ^ Section
    -> IO (AbortOr (Kit.Section v))  -- ^ Union of sections
readSec sec =
    do let sec1 = map (Kit.sectionRead $ rootSection sec) $ textSections sec
       sec2   <- mapM (Kit.sectionFile $ rootSection sec) $ fileSections sec
       return $ concatMM $ sec1 ++ sec2


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Toolkit.Library.Run
( SectionSource (..),
  runFiles,
  hRunFiles,
  concatMM,
  runCalc,
  runCalcTo,
  theContent,
  readSec,
  readSecList,
  mkdir,
) where

import Data.Monoid
import qualified System.IO        as IO
import qualified System.FilePath  as Path
import qualified System.Directory as Dir

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Toolkit.Library.Comment


data SectionSource c = SectionSource
    { rootSection  :: C.Section c
    , textSections :: [String]
    , fileSections :: [FilePath]
    } deriving (Show)
      

-- ----------------------

runFiles :: (C.CContent c) => SectionSource c -> IO ()
runFiles = hRunFiles IO.stdout

{-| Read and union sections from files, and run the section. -}
hRunFiles
    :: (C.CContent c)
    => IO.Handle       -- ^ File handle
    -> SectionSource c -- ^ Section source code
    -> IO ()
hRunFiles h (SectionSource root textSec files) =
    do let sec = map (C.sectionRead root "") $ textSec
       sects <- mapM (C.sectionFile root) $ files
       let union = concatMM $ sec ++ sects
           comm  = CommentDoc
                   [ CommentSec "INPUT" files]
       IO.hSetEncoding h IO.utf8
       IO.hPutStrLn    h emacsModeComment
       IO.hPutStr      h $ unlines $ texts comm
       IO.hPutStrLn    h ""
       B.abortIO (C.hRunSectionIO h) union

concatMM :: (Monad m, Monoid a) => [m a] -> m a
concatMM [] = return mempty
concatMM (s:ss) = do s'  <- s
                     ss' <- concatMM ss
                     return $ mappend s' ss'



-- ---------------------- Calculation list

runCalc :: (C.CContent c) => SectionSource c -> IO ()
runCalc = runCalcTo ""

runCalcTo
    :: (C.CContent c)
    => FilePath          -- ^ Output path prefix
    -> SectionSource c   -- ^ Section
    -> IO ()             -- ^
runCalcTo dir sec =
    do union <- readSec sec
       B.abortIO (runCalcSec dir $ rootSection sec) union

runCalcSec :: (C.CContent c) => String -> C.Section c -> C.Section c -> IO ()
runCalcSec dir root sec =
    do let js = C.sectionJudge sec
       mapM_ (runCalcJudge dir root) js
       return ()

runCalcJudge :: (C.CContent c) => String -> C.Section c -> B.Judge c -> IO ()
runCalcJudge dir root (B.Judge True "KOSHU-CALC" xs) =
    case theContents ["/input", "/output"] xs of
      Just [input, output] ->
          do let inputFiles = theStrings input
                 outputFile = dir ++ C.getText output
             putStrLn $ "**  Output to " ++ outputFile
             mkdir outputFile
             IO.withFile outputFile IO.WriteMode
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

theContent :: (C.CContent c) => String -> [B.Named c] -> Maybe c
theContent = lookup

theContents :: (C.CContent c) => [String] -> [B.Named c] -> Maybe [c]
theContents ns termset = mapM (`theContent` termset) ns

theStrings :: (C.CContent c) => c -> [String]
theStrings c | C.isText c  =  [C.getText c]
theStrings c | C.isList c  =  map C.getText $ C.getList c
theStrings _               =  []



-- ----------------------  Read

readSec
    :: (C.CContent c)
    => SectionSource c               -- ^ Section
    -> IO (B.AbortOr (C.Section c))  -- ^ Union of sections
readSec src =
    do let root = rootSection src
           sec1 = map (C.sectionRead root "") $ textSections src
       sec2   <- mapM (C.sectionFile root) $ fileSections src
       return $ concatMM $ sec1 ++ sec2

readSecList
    :: (C.CContent c)
    => SectionSource c
    -> IO (B.AbortOr [C.Section c])
readSecList src =
    do let root = rootSection src
           sec1 = map (C.sectionRead root "") $ textSections src
       sec2 <- mapM (C.sectionFile root) $ fileSections src
       return $ sequence $ sec1 ++ sec2


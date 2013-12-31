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

import qualified Data.Monoid          as M
import qualified System.IO            as IO
import qualified System.FilePath      as Path
import qualified System.Directory     as Dir
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C


data SectionSource c = SectionSource
    { rootSection  :: C.Section c
    , textSections :: [String]
    , fileSections :: [FilePath]
    } deriving (Show)
      

-- ----------------------

runFiles :: (C.CContent c) => B.CommandLine -> SectionSource c -> IO Int
runFiles = hRunFiles IO.stdout

{-| Read and union sections from files, and run the section. -}
hRunFiles
    :: (C.CContent c)
    => IO.Handle        -- ^ File handle
    -> B.CommandLine    -- ^ Command line
    -> SectionSource c  -- ^ Section source code
    -> IO Int
hRunFiles h args (SectionSource root textSec files) =
    do let sec = map (C.readSectionText root) textSec
       sects <- mapM (C.readSectionFile root) files
       let union = concatMM $ sec ++ sects
           comm  = B.CommentDoc [ B.CommentSec "INPUT" files ]

       IO.hSetEncoding h IO.utf8
       IO.hPutStrLn    h B.emacsModeComment
       IO.hPutStr      h $ unlines $ B.texts comm
       IO.hPutStrLn    h ""

       B.abortMap args (C.hPutSection h) $ C.runSection =<< union

concatMM :: (Monad m, M.Monoid a) => [m a] -> m a
concatMM [] = return M.mempty
concatMM (s:ss) =
    do s'  <- s
       ss' <- concatMM ss
       return $ M.mappend s' ss'



-- ---------------------- Calculation list

runCalc :: (C.CContent c) => SectionSource c -> IO Int
runCalc = runCalcTo ""

runCalcTo
    :: (C.CContent c)
    => FilePath          -- ^ Output path prefix
    -> SectionSource c   -- ^ Section
    -> IO Int            -- ^
runCalcTo dir sec =
    do union <- readSec sec
       B.abortMap [] (runCalcSec dir $ rootSection sec) union

runCalcSec :: (C.CContent c) => String -> C.Section c -> C.Section c -> IO Int
runCalcSec dir root sec =
    do let js = C.sectionJudge sec
       mapM_ (runCalcJudge dir root) js
       return 0

runCalcJudge :: (C.CContent c) => String -> C.Section c -> B.Judge c -> IO Int
runCalcJudge dir root (B.Judge True "KOSHU-CALC" xs) =
    case theContents ["/input", "/output"] xs of
      Just [input, output] ->
          do let inputFiles = theStrings input
                 outputFile = dir ++ C.getText output
             putStrLn $ "**  Output to " ++ outputFile
             mkdir outputFile
             IO.withFile outputFile IO.WriteMode
                          $ \ h -> hRunFiles h []
                                   (SectionSource root [] inputFiles)
      Just _       -> return 0
      Nothing      -> return 0
runCalcJudge _ _ _ =  return 0

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
    -> IO (B.Ab (C.Section c))  -- ^ Union of sections
readSec src =
    do let root = rootSection src
           sec1 = map (C.readSectionText root) $ textSections src
       sec2   <- mapM (C.readSectionFile root) $ fileSections src
       return $ concatMM $ sec1 ++ sec2

readSecList
    :: (C.CContent c)
    => SectionSource c
    -> IO (B.Ab [C.Section c])
readSecList src =
    do let root = rootSection src
           sec1 = map (C.readSectionText root) $ textSections src
       sec2 <- mapM (C.readSectionFile root) $ fileSections src
       return $ sequence $ sec1 ++ sec2


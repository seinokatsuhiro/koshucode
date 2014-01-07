{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Toolkit.Library.Run
( runFiles,
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



-- ----------------------

runFiles :: (C.CContent c) => C.Global c -> C.SectionBundle c -> IO Int
runFiles = hRunFiles IO.stdout

{-| Read and union sections from files, and run the section. -}
hRunFiles
    :: (C.CContent c)
    => IO.Handle          -- ^ File handle
    -> C.Global c         -- ^ Global parameters
    -> C.SectionBundle c  -- ^ Section source code
    -> IO Int
hRunFiles h global src =
    do sects <- C.readSectionBundle src
       let union = concatMM sects
           files = C.bundleFiles src
           comm  = B.CommentDoc [ B.CommentSec "INPUT" files ]

       IO.hSetEncoding h IO.utf8
       IO.hPutStrLn    h B.emacsModeComment
       IO.hPutStr      h $ unlines $ B.texts comm
       IO.hPutStrLn    h ""

       let cmd = C.globalCommandLine global
       B.abortableIO cmd (C.hPutSection h) $ C.runSection global =<< union

concatMM :: (Monad m, M.Monoid a) => [m a] -> m a
concatMM [] = return M.mempty
concatMM (s:ss) =
    do s'  <- s
       ss' <- concatMM ss
       return $ M.mappend s' ss'



-- ---------------------- Calculation list

runCalc :: (C.CContent c) => B.CommandLine -> C.SectionBundle c -> IO Int
runCalc = runCalcTo ""

runCalcTo
    :: (C.CContent c)
    => FilePath          -- ^ Output path prefix
    -> B.CommandLine
    -> C.SectionBundle c   -- ^ Section
    -> IO Int
runCalcTo dir cmd sec =
    do union <- readSec sec
       B.abortableIO cmd (runCalcSec dir $ C.bundleRoot sec) union

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
                          $ \ h -> hRunFiles h C.global
                                   (C.SectionBundle root [] inputFiles [])
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

readSec :: (C.CContent c) => C.SectionBundle c -> IO (B.Ab (C.Section c))
readSec src =
    do sects <- C.readSectionBundle src
       return $ concatMM $ sects

readSecList :: (C.CContent c) => C.SectionBundle c -> IO (B.Ab [C.Section c])
readSecList src =
    do sects <- C.readSectionBundle src
       return $ sequence $ sects


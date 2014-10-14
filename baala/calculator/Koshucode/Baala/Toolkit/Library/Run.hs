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

import qualified System.IO            as IO
import qualified System.FilePath      as Path
import qualified System.Directory     as Dir
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C



-- ----------------------

runFiles :: (C.CContent c) => C.Global c -> C.SectionBundle c -> IO Int
runFiles = hRunFiles IO.stdout

-- | Read and union sections from files, and run the section.
hRunFiles
    :: (C.CContent c)
    => IO.Handle          -- ^ Output file handle
    -> C.Global c         -- ^ Global parameters
    -> C.SectionBundle c  -- ^ Section source code
    -> IO Int
hRunFiles h global bun =
    do abSects <- C.readSectionBundle bun
       let inputs  =  C.bundleTexts bun
           comm    =  B.CommentDoc [ B.CommentSec "INPUT" inputs ]

       IO.hSetEncoding h IO.utf8
       IO.hPutStrLn    h B.emacsModeComment
       IO.hPutStr      h $ unlines $ B.texts comm
       IO.hPutStrLn    h ""

       let cmd = C.globalCommandLine global
           js' = do sects <- B.sequence abSects
                    C.runSection global sects

       case js' of
         Left a   -> B.abort cmd a
         Right js -> B.hPutOutputResult h js



-- ---------------------- Calculation list

runCalc :: (C.CContent c) => B.CommandLine -> C.SectionBundle c -> IO Int
runCalc = runCalcTo ""

runCalcTo :: (C.CContent c) =>
  FilePath -> B.CommandLine -> C.SectionBundle c -> IO Int
runCalcTo dir cmd bundle =
    do union <- readSec bundle
       case union of
         Left a    -> B.abort cmd a
         Right sec -> runCalcSec dir (C.bundleRoot bundle) sec

runCalcSec :: (C.CContent c) => String -> C.Section c -> C.Section c -> IO Int
runCalcSec dir root sec =
    do let js = C.secJudge sec
       mapM_ (runCalcJudge dir root) js
       return 0

runCalcJudge :: (C.CContent c) => String -> C.Section c -> B.Judge c -> IO Int
runCalcJudge dir root (B.JudgeAffirm "KOSHU-CALC" xs) =
    case theContents ["/input", "/output"] xs of
      Just [input, output] ->
          do let inputFiles = theStrings input
                 outputFile = dir ++ C.gText output
             putStrLn $ "**  Output to " ++ outputFile
             mkdir outputFile
             IO.withFile outputFile IO.WriteMode
                          $ \ h -> do let res = B.resourceList False [] inputFiles []
                                      hRunFiles h C.global $ C.SectionBundle root res
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
theContents ns assn = mapM (`theContent` assn) ns

theStrings :: (C.CContent c) => c -> [String]
theStrings c | C.isText c  =  [C.gText c]
theStrings c | C.isList c  =  map C.gText $ C.gList c
theStrings _               =  []



-- ----------------------  Read

readSec :: (C.CContent c) => C.SectionBundle c -> IO (B.Ab (C.Section c))
readSec bun =
    do sects <- C.readSectionBundle bun
       return $ concatMM $ sects

readSecList :: (C.CContent c) => C.SectionBundle c -> IO (B.Ab [C.Section c])
readSecList bun =
    do sects <- C.readSectionBundle bun
       return $ sequence $ sects

concatMM :: (Monad m, B.Monoid a) => [m a] -> m a
concatMM [] = return B.mempty
concatMM (s:ss) =
    do s'  <- s
       ss' <- concatMM ss
       return $ B.mappend s' ss'

{-# OPTIONS_GHC -Wall #-}

{-| Tool for doing regression tests. -}

module Koshucode.Baala.Toolkit.Main.KoshuRegress
( koshuRegressMain

  -- * koshu-regress.hs
  -- $main
) where

import Control.Monad
import System.Console.GetOpt
import System.IO
import Data.Maybe (mapMaybe)
import qualified System.Directory as Dir

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Vanilla

import Koshucode.Baala.Toolkit.Library.Comment
import Koshucode.Baala.Toolkit.Library.Change
import Koshucode.Baala.Toolkit.Library.Exit
import Koshucode.Baala.Toolkit.Library.Input
import Koshucode.Baala.Toolkit.Library.Run
import Koshucode.Baala.Toolkit.Library.Version



-- ----------------------  Option

data Option
    = OptHelp
    | OptVersion
    | OptRun
    | OptShowEncoding
    | OptSave            -- save the last result
    | OptClean           -- clean all result
    | OptReport          -- report changes between last and save
      deriving (Show, Eq, Ord, Enum, Bounded)

koshuOptions :: [OptDescr Option]
koshuOptions =
    [ Option "h"  ["help"]     (NoArg OptHelp)    "Print help message."
    , Option "V"  ["version"]  (NoArg OptVersion) "Print version number."
    , Option ""   ["run"]      (NoArg OptRun)     "Calculate and report"
    , Option ""   ["show-encoding"] (NoArg OptShowEncoding) "Show character encoding."
    , Option "s"  ["save"]     (NoArg OptSave)    "Save result."
    , Option "r"  ["report"]   (NoArg OptReport)  "Report last result."
    , Option "c"  ["clean"]    (NoArg OptClean)   "Remove output directory."
    ]

version :: String
version = "koshu-regress-" ++ versionString

usage :: String
usage = usageInfo header koshuOptions

header :: String
header = unlines
    [ "DESCRIPTION"
    , "  Execute regression test."
    , ""
    , "USAGE"
    , "  koshu-regress CALC.k [OPTION]"
    , ""
    , "  CALC.k contains KOSHU-CALC judges like"
    , "  |-- KOSHU-CALC /input FILES /output FILE"
    , ""
    ] ++ "OPTIONS"



-- ----------------------  Directories

regressDir :: String
regressDir = "REGRESS/"

lastDir    :: String
lastDir    = "REGRESS/last/"

saveDir    :: String
saveDir    = "REGRESS/save/"

reportDir  :: String
reportDir  = "REGRESS/report/"



-- ----------------------  Main

{-| The main function for @koshu-regress@ command. -}
koshuRegressMain :: (C.CContent c) => [C.Rop c] -> IO ()
koshuRegressMain relmaps =
  let cons = C.relmapCons relmaps
      root = C.makeEmptySection cons
  in koshuRegressMain' root =<< prelude

koshuRegressMain'
    :: (C.CContent c) => C.Section c -> (String, [String]) -> IO ()
koshuRegressMain' root (_, argv) =
    case getOpt Permute koshuOptions argv of
      (opts, files, [])
          | has OptHelp         -> putSuccess usage
          | has OptVersion      -> putSuccess $ version ++ "\n"
          | has OptShowEncoding -> putSuccess =<< currentEncodings
          | has OptSave         -> regSave
          | has OptClean        -> regClean
          | has OptReport       -> regReport sec
          | has OptRun          -> regLastReport sec
          | otherwise           -> regLast sec
          where has = (`elem` opts)
                sec = SectionSource root [] files
      (_, _, errs) -> putFailure $ concat errs ++ usage

regLast :: (C.CContent c) => SectionSource c -> IO ()
regLast sec = runCalcTo lastDir sec

regLastReport :: (C.CContent c) => SectionSource c -> IO ()
regLastReport sec =
    do regLast sec
       regReport sec

-- mv last save
regSave :: IO ()
regSave =
    do e <- Dir.doesDirectoryExist saveDir
       when e $ Dir.removeDirectoryRecursive saveDir
       Dir.renameDirectory lastDir saveDir

-- rm -r REGRESS
regClean :: IO ()
regClean =
    do e <- Dir.doesDirectoryExist regressDir
       when e $ Dir.removeDirectoryRecursive regressDir



-- ----------------------  Reporting

-- report for each cases.
reportJudge :: [B.Named VContent] -> B.Judge VContent
reportJudge = B.Judge True "KOSHU-REGRESS-REPORT"

-- report for all cases.
summaryJudge :: [B.Named VContent] -> B.Judge VContent
summaryJudge = B.Judge True "KOSHU-REGRESS-SUMMARY"

reportHead :: CommentDoc
reportHead =
    CommentDoc
    [ CommentSec "DESCRIPTOIN"
      [ "Result of a regression test" ]]

reportFoot :: String -> IO ()
reportFoot msg = foot where
    foot = putStr $ unlines [ "" , comm , comm ++ msg , comm ]
    comm = "**  "

regReport :: (C.CContent c) => SectionSource c -> IO ()
regReport sec =
    do putStrLn emacsModeComment
       putStr . unlines $ texts reportHead
       putStrLn ""
       asec <- readSec sec
       case asec of
         Left _     -> putStrLn "error"
         Right sec2 -> regReportBody sec2

regReportBody :: (C.CContent c) => C.Section c -> IO ()
regReportBody sec =
    do let js = C.sectionJudge sec
           fs = mapMaybe outputFile js
       bs <- mapM reportFile fs
       let match   = filter id  bs
           unmatch = filter not bs
       putStrLn ""
       putDoc $ summaryJudge
         [ ("/all"     , theLength fs)
         , ("/match"   , theLength match)
         , ("/unmatch" , theLength unmatch) ]

       case unmatch of
         [] -> reportFoot $ "Matched"
         _  -> reportFoot $ "Please check report files in " ++ reportDir

outputFile :: (C.CContent c) => B.Judge c -> Maybe String
outputFile jud =
    do (B.Judge _ _ xs) <- judgeOf "KOSHU-CALC" jud
       output <- theContent "/output" xs
       Just $ C.getText output

reportFile :: String -> IO Bool
reportFile file =
    do let saveFile = File $ saveDir ++ file
           lastFile = File $ lastDir ++ file
       js <- lastFile `minusInputJudge` saveFile

       case countAffirmDeny js of
         (0, 0) -> reportMatch file
         count  -> reportUnmatch file js count

reportMatch :: String -> IO Bool
reportMatch file =
    do putDoc $ reportJudge
         [ ("/result" , C.putBool True)
         , ("/output" , C.putText file) ]
       return True

reportUnmatch
    :: (Ord c, B.Pretty c) =>
       String -> [B.Judge c] -> (Int, Int) -> IO Bool
reportUnmatch file js (add, del) =
    do putDoc $ reportJudge
         [ ("/result" , C.putBool False)
         , ("/output" , C.putText file) ]
       putStrLn $ "    **  " ++ reportCount add del
       let path = reportDir ++ file
       mkdir path
       writeJudgesToFile path js
       return False

writeJudgesToFile :: (Ord c, B.Pretty c) => FilePath -> [B.Judge c] -> IO ()
writeJudgesToFile path js =
    withFile path WriteMode writer where
    writer h = do hSetEncoding h utf8
                  B.hPutJudges h js

reportCount :: Int -> Int -> String
reportCount = message where
    message a 0  =  addition a
    message 0 d  =  deletion d
    message a d  =  addition a ++ ", " ++ deletion d

    addition 1   =  "1 addition"
    addition n   =  show n ++ " additions"

    deletion 1   =  "1 deletion"
    deletion n   =  show n ++ " deletions"



-- ----------------------  Utility

{-| Length of list as an integer content. -}
theLength :: (C.CDec c) => [a] -> c
theLength = C.putDecFromInt . length

putDoc :: (B.Pretty p) => p -> IO ()
putDoc = print . B.doc

judgeOf :: String -> B.Judge c -> Maybe (B.Judge c)
judgeOf pat1 j@(B.Judge _ pat2 _)
        | pat1 == pat2  =  Just j
        | otherwise     =  Nothing

countAffirmDeny :: [B.Judge c] -> (Int, Int)
countAffirmDeny js =
    ( length $ filter B.isAffirmed js
    , length $ filter B.isDenied js )



-- ----------------------
{- $main

   @koshu-regress@ command is implemented using 'koshuRegressMain'.

   > import Koshucode.Baala.Toolkit.Main.KoshuRegress
   > import Koshucode.Baala.Vanilla as V
   > 
   > main :: IO ()
   > main = koshuRegressMain V.vanillaRops
-}


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

import qualified Koshucode.Baala.Base   as B
import qualified Koshucode.Baala.Core   as C
import qualified Koshucode.Baala.Op     as Op

import qualified Koshucode.Baala.Toolkit.Library.Change   as L
import qualified Koshucode.Baala.Toolkit.Library.Exit     as L
import qualified Koshucode.Baala.Toolkit.Library.Input    as L
import qualified Koshucode.Baala.Toolkit.Library.Run      as L
import qualified Koshucode.Baala.Toolkit.Library.Version  as L



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
version = "koshu-regress-" ++ L.versionString

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
koshuRegressMain :: (C.CContent c) => [C.Rop c] -> IO Int
koshuRegressMain relmaps =
  let cons = C.relmapCons $ C.global { C.globalRops = relmaps }
      root = C.makeEmptySection cons
  in koshuRegressMain' root =<< L.prelude

koshuRegressMain'
    :: (C.CContent c) => C.Section c -> (String, [String]) -> IO Int
koshuRegressMain' root (prog, argv) =
    case getOpt Permute koshuOptions argv of
      (opts, files, [])
          | has OptHelp         -> L.putSuccess usage
          | has OptVersion      -> L.putSuccess $ version ++ "\n"
          | has OptShowEncoding -> L.putSuccess =<< L.currentEncodings
          | has OptSave         -> regSave
          | has OptClean        -> regClean
          | has OptReport       -> regReport sec
          | has OptRun          -> regLastReport sec
          | otherwise           -> regLast cmd sec
          where has = (`elem` opts)
                sec = C.SectionBundle root [] files []
                cmd = prog : argv
      (_, _, errs) -> L.putFailure $ concat errs ++ usage

regLast :: (C.CContent c) => B.CommandLine -> C.SectionBundle c -> IO Int
regLast cmd sec = L.runCalcTo lastDir cmd sec

regLastReport :: (C.CContent c) => C.SectionBundle c -> IO Int
regLastReport sec =
    do _ <- regLast [] sec
       regReport sec

-- mv last save
regSave :: IO Int
regSave =
    do e <- Dir.doesDirectoryExist saveDir
       when e $ Dir.removeDirectoryRecursive saveDir
       Dir.renameDirectory lastDir saveDir
       return 0

-- rm -r REGRESS
regClean :: IO Int
regClean =
    do e <- Dir.doesDirectoryExist regressDir
       when e $ Dir.removeDirectoryRecursive regressDir
       return 0



-- ----------------------  Reporting

-- report for each cases.
reportJudge :: [B.Named Op.VContent] -> B.Judge Op.VContent
reportJudge = B.Judge True "KOSHU-REGRESS-REPORT"

-- report for all cases.
summaryJudge :: [B.Named Op.VContent] -> B.Judge Op.VContent
summaryJudge = B.Judge True "KOSHU-REGRESS-SUMMARY"

reportHead :: B.CommentDoc
reportHead =
    B.CommentDoc
    [ B.CommentSec "DESCRIPTOIN"
      [ "Result of a regression test" ]]

reportFoot :: String -> IO ()
reportFoot msg = foot where
    foot = putStr $ unlines [ "" , comm , comm ++ msg , comm ]
    comm = "**  "

regReport :: (C.CContent c) => C.SectionBundle c -> IO Int
regReport sec =
    do putStrLn B.emacsModeComment
       putStr . unlines $ B.texts reportHead
       putStrLn ""
       asec <- L.readSec sec
       case asec of
         Left _     -> putStrLn "error"
         Right sec2 -> regReportBody sec2
       return 0

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
       output <- L.theContent "/output" xs
       Just $ C.gText output

reportFile :: String -> IO Bool
reportFile file =
    do let saveFile = L.File $ saveDir ++ file
           lastFile = L.File $ lastDir ++ file
       js <- lastFile `L.minusInputJudge` saveFile

       case countAffirmDeny js of
         (0, 0) -> reportMatch file
         count  -> reportUnmatch file js count

reportMatch :: String -> IO Bool
reportMatch file =
    do putDoc $ reportJudge
         [ ("/result" , C.pBool True)
         , ("/output" , C.pText file) ]
       return True

reportUnmatch
    :: (Ord c, B.Pretty c) =>
       String -> [B.Judge c] -> (Int, Int) -> IO Bool
reportUnmatch file js (add, del) =
    do putDoc $ reportJudge
         [ ("/result" , C.pBool False)
         , ("/output" , C.pText file) ]
       putStrLn $ "    **  " ++ reportCount add del
       let path = reportDir ++ file
       L.mkdir path
       _ <- writeJudgesToFile path js
       return False

writeJudgesToFile :: (Ord c, B.Pretty c) => FilePath -> [B.Judge c] -> IO Int
writeJudgesToFile path js =
    withFile path WriteMode writer where
    writer h = do hSetEncoding h utf8
                  B.hPutJudgesFlat h 0 js

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
theLength = C.pDecFromInt . length

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
   > import Koshucode.Baala.Op.Vanilla as V
   > 
   > main :: IO ()
   > main = koshuRegressMain V.vanillaRops
-}


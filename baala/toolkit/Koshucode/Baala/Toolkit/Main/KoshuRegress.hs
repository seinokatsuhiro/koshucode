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

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Vanilla.Value.Val

import qualified Koshucode.Baala.Base.Section  as Kit
import qualified Koshucode.Baala.Minimal.OpKit as Kit

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
    | OptSave
    | OptClean
    | OptReport
      deriving (Show, Eq, Ord, Enum, Bounded)

koshuOptions :: [OptDescr Option]
koshuOptions =
    [ Option "h" ["help"]     (NoArg OptHelp)    "Print help message."
    , Option "V" ["version"]  (NoArg OptVersion) "Print version number."
    , Option ""  ["run"]      (NoArg OptRun)     "Calculate and report"
    , Option ""  ["show-encoding"] (NoArg OptShowEncoding) "Show character encoding."
    , Option ""  ["save"]     (NoArg OptSave)    "Save result."
    , Option ""  ["clean"]    (NoArg OptClean)   "Remove output directory."
    , Option ""  ["report"]   (NoArg OptReport)  "Report last result."
    ]

version :: String
version = "koshu-regress-" ++ versionString

usage :: String
usage = usageInfo header koshuOptions

header :: String
header = unlines
    [ "DESCRIPTION"
    , "  Calculate changeset between two dataset."
    , ""
    , "USAGE"
    , "  koshu-regress REGRESS.k"
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
koshuRegressMain :: (Value v) => [Kit.OpImplement v] -> IO ()
koshuRegressMain relmaps =
  let cons = Kit.relmapCons relmaps
      root = Kit.makeEmptySection cons
  in koshuRegressMain' root =<< prelude

koshuRegressMain' :: (Value v) => Kit.Section v -> (String, [String]) -> IO ()
koshuRegressMain' root (_, argv) =
    case getOpt Permute koshuOptions argv of
      (opts, files, [])
          | has OptHelp         -> putSuccess usage
          | has OptVersion      -> putSuccess $ version ++ "\n"
          | has OptShowEncoding -> putSuccess =<< currentEncodings
          | has OptSave         -> save
          | has OptClean        -> clean
          | has OptReport       -> report    sec
          | has OptRun          -> runReport sec
          | otherwise           -> run       sec
          where has = (`elem` opts)
                sec = SectionSource root [] files
      (_, _, errs) -> putFailure $ concat errs ++ usage

run :: (Value v) => SectionSource v -> IO ()
run sec = runCalcTo lastDir sec

runReport :: (Value v) => SectionSource v -> IO ()
runReport sec =
    do run    sec
       report sec

save :: IO ()
save = do e <- Dir.doesDirectoryExist saveDir
          when e $ Dir.removeDirectoryRecursive saveDir
          Dir.renameDirectory lastDir saveDir

clean :: IO ()
clean = do e <- Dir.doesDirectoryExist regressDir
           when e $ Dir.removeDirectoryRecursive regressDir

report :: (Value v) => SectionSource v -> IO ()
report sec =
    do asec <- readSec sec
       case asec of
         Left _ -> putStrLn "error"
         Right sec2 -> do let js = Kit.sectionJudge sec2
                              fs = mapMaybe outputFile js
                          mapM_ reportFile fs

outputFile :: (Value v) => Judge v -> Maybe String
outputFile (Judge True "KOSHU-CALC" xs) =
    case theContent "/output" xs of
      Nothing -> Nothing
      Just f  -> Just $ theStringValue f
outputFile (Judge _ _ _) = Nothing

reportFile :: String -> IO ()
reportFile file =
    do let saveFile = File $ saveDir ++ file
           lastFile = File $ lastDir ++ file
       js <- minusInputJudge saveFile lastFile

       case length js of
         0 -> do print . doc $ reportJudge
                      [ ("/result" , boolValue True)
                      , ("/file"   , stringValue file) ]

         n -> do putStrLn $ "**  Difference found."
                 print . doc $ reportJudge
                      [ ("/result" , boolValue False)
                      , ("/file"   , stringValue file)
                      , ("/count"  , intValue n) ]

                 let path = reportDir ++ file
                 mkdir path
                 withFile path WriteMode
                      $ \ h -> hPutJudges h js

reportJudge :: [Named Val] -> Judge Val
reportJudge xs = Judge True "KOSHU-REPORT" xs


-- ----------------------
-- $main
--
-- @koshu-regress@ command is implemented using 'koshuRegressMain'.
--
-- > import Koshucode.Baala.Toolkit.Main.KoshuRegress
-- > import Koshucode.Baala.Vanilla as V
-- > 
-- > main :: IO ()
-- > main = koshuRegressMain V.vanillaOperators


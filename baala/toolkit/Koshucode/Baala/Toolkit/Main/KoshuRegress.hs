{-# OPTIONS_GHC -Wall #-}

{-| Tool for doing regression tests. -}

module Koshucode.Baala.Toolkit.Main.KoshuRegress
( koshuRegressMain

-- * koshu-regress.hs
-- $main
) where

import Control.Monad
import System.Console.GetOpt
import qualified System.Directory as Dir

import Koshucode.Baala.Base.Data

import qualified Koshucode.Baala.Base.Section  as Kit
import qualified Koshucode.Baala.Minimal.OpKit as Kit

import Koshucode.Baala.Toolkit.Library.Exit
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
      deriving (Show, Eq, Ord, Enum, Bounded)

koshuOptions :: [OptDescr Option]
koshuOptions =
    [ Option "h" ["help"]     (NoArg OptHelp)    "Print help message."
    , Option "V" ["version"]  (NoArg OptVersion) "Print version number."
    , Option ""  ["run"]      (NoArg OptRun)     "Run section."
    , Option ""  ["show-encoding"] (NoArg OptShowEncoding) "Show character encoding."
    , Option ""  ["save"]     (NoArg OptSave)    "Save result."
    , Option ""  ["clean"]    (NoArg OptClean)   "Remove output directory."
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
          | otherwise           -> run root files
          where has = (`elem` opts)
      (_, _, errs) -> putFailure $ concat errs ++ usage

regressDir :: String
regressDir = "REGRESS/"

lastDir    :: String
lastDir    = "REGRESS/last/"

saveDir    :: String
saveDir    = "REGRESS/save/"

run :: (Value v) => Kit.Section v -> [FilePath] -> IO ()
run root files = runCalcTo lastDir root [] files

save :: IO ()
save = do e <- Dir.doesDirectoryExist saveDir
          when e $ Dir.removeDirectoryRecursive saveDir
          Dir.renameDirectory lastDir saveDir

clean :: IO ()
clean = do e <- Dir.doesDirectoryExist regressDir
           when e $ Dir.removeDirectoryRecursive regressDir




-- ----------------------
-- $main
--
-- @koshu-regress@ command is implemented using 'koshuRegressMain'.
--
-- > import Koshucode.Baala.Toolkit.Main.KoshuRegress
-- > 
-- > main :: IO ()
-- > main = koshuRegressMain


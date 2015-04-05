{-# OPTIONS_GHC -Wall #-}

-- | Koshu filter program.

module Koshucode.Baala.Toolkit.Main.KoshuFilter
  ( koshuFilter
  ) where

import qualified System.Console.GetOpt                 as G
import qualified Data.Time                             as T
import qualified System.IO                             as IO
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core                  as C
import qualified Koshucode.Baala.Toolkit.Library.Run   as L
import qualified Koshucode.Baala.Toolkit.Library.Exit  as L

data Option
    = OptHelp
    | OptVersion
    | OptStdin
      deriving (Show, Eq)

options :: [G.OptDescr Option]
options =
    [ G.Option "h" ["help"]     (G.NoArg OptHelp)    "Print help message."
    , G.Option "V" ["version"]  (G.NoArg OptVersion) "Print version number."
    , G.Option "i" ["stdin"]    (G.NoArg OptStdin)   "Read from stdin."
    ]

usage :: String
usage = G.usageInfo header options

header :: String
header = unlines
    [ "DESCRIPTION"
    , "  Koshu filter program."
    , ""
    , "USAGE"
    , "  koshu FILE.k ...               Process FILE.k ..."
    , "  koshu -i FILE.k ... < FILE.k   Read from stdin"
    , ""
    ] ++ "OPTIONS"

koshuFilter :: (C.CContent c) => C.Global c -> (C.Resource c -> IO Int) -> IO Int
koshuFilter g withRes =
  do (prog, argv) <- L.prelude
     proxy        <- L.getProxies
     time         <- T.getZonedTime
     let day       = T.localDay $ T.zonedTimeToLocalTime time
     case G.getOpt G.Permute options argv of
       (opts, paths, [])
           | has OptHelp     -> L.putSuccess usage
           | has OptVersion  -> L.putSuccess $ ver ++ "\n"
           | otherwise       -> runFiles g2 withRes src
           where
             has   = (`elem` opts)
             ver   = C.globalSynopsis g ++ " " ++ C.globalVersionText g
             root  = C.resEmpty { C.resGlobal = g2 }
             src   = B.ioPointList (has OptStdin) [] paths
             g2    = C.globalFill g
                       { C.globalProgram   = prog
                       , C.globalArgs      = argv
                       , C.globalProxy     = proxy
                       , C.globalTime      = B.timeYmd day
                       , C.globalHook      = root }

       (_, _, errs) -> L.putFailure $ concat errs

runFiles :: (C.CContent c) => C.Global c -> (C.Resource c -> IO Int) -> [B.IOPoint] -> IO Int
runFiles = hRunFiles IO.stdout

hRunFiles
    :: (C.CContent c)
    => IO.Handle                 -- ^ Output file handler
    -> C.Global c                -- ^ Global parameters
    -> (C.Resource c -> IO Int)  -- ^ Resource handler
    -> [B.IOPoint]               -- ^ Names of source codes
    -> IO Int
hRunFiles h g withRes ns =
    do (abRes, _) <- C.gioResource (C.readSources ns) g
       IO.hSetEncoding h IO.utf8
       case abRes of
         Left  a   -> C.globalAbort g a
         Right res -> withRes res


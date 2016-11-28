{-# OPTIONS_GHC -Wall #-}

-- | Exit process.

module Koshucode.Baala.System.Exit
  ( exit, exitCode,
    putSuccess, putSuccessLn,
    putAbort, putAbortWith,
    putFailure, putFailureLn,
  ) where

import qualified System.Environment            as Sys
import qualified System.Exit                   as Exit
import qualified System.IO                     as IO

-- | Exit process.
exit :: Int -> IO a
exit = Exit.exitWith . exitCode

-- | Map integer value to exit code.
exitCode :: Int -> Exit.ExitCode
exitCode 0 = Exit.ExitSuccess
exitCode n = Exit.ExitFailure n

-- | Print message and exit on 0.
putSuccess :: String -> IO a
putSuccess msg = success $ putStr msg

-- | Print message line and exit on 0.
putSuccessLn :: String -> IO a
putSuccessLn msg = success $ putStrLn msg

-- | Print abort message and exit on 1.
--
--   >>> putAbort
--   ABORT / <interactive> exits on 1
--
putAbort :: IO a
putAbort =
    do prog <- Sys.getProgName
       putFailureLn $ "ABORT / " ++ prog ++ " exits on 1"

-- | Print error and abort message and exit on 1.
putAbortWith :: String -> IO a
putAbortWith msg =
    do IO.hPutStrLn IO.stderr msg
       putAbort

-- | Print error message and exit on 1.
putFailure :: String -> IO a
putFailure msg = failure $ IO.hPutStr IO.stderr msg

-- | Print error message line and exit on 1.
putFailureLn :: String -> IO a
putFailureLn msg = failure $ IO.hPutStrLn IO.stderr msg

success :: IO () -> IO a
success body = body >> exit 0

failure :: IO () -> IO a
failure body = body >> exit 1


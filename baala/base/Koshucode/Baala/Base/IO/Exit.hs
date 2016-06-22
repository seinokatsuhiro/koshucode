{-# OPTIONS_GHC -Wall #-}

-- | Exit process.

module Koshucode.Baala.Base.IO.Exit
  ( progAndArgs,
    hSetKoshuOutput,
    currentEncodings,
    exit, exitCode,
    putSuccess, putSuccessLn,
    putFailure, putFailureLn,
  ) where

import qualified GHC.IO.Encoding               as Enc
import qualified System.Environment            as Sys
import qualified System.Exit                   as Exit
import qualified System.IO                     as IO
import qualified Koshucode.Baala.Base.Prelude  as B


-- | Program name and command-line arguments.
--   This function removes carrige returns from the arguments.
progAndArgs :: IO (String, [String])
progAndArgs =
    do hSetKoshuOutput B.stdout
       prog <- Sys.getProgName
       args <- Sys.getArgs
       return (prog, deleteCr args)

-- Delete carriage return from command line argument.
deleteCr :: [String] -> [String]
deleteCr = map (filter notCrChar) . filter notCrString where
    notCrChar   = (/= '\r')
    notCrString = (/= "\r")

-- | Set I/O handle for Koshucode output.
hSetKoshuOutput :: IO.Handle -> IO ()
hSetKoshuOutput h =
    do Enc.setLocaleEncoding Enc.utf8_bom
       IO.hSetNewlineMode h koshuNewlineMode
       IO.hSetEncoding h IO.utf8

-- | CRLF input and CRLF output.
koshuNewlineMode :: IO.NewlineMode
koshuNewlineMode = IO.universalNewlineMode { IO.inputNL  = IO.CRLF
                                           , IO.outputNL = IO.CRLF }

-- | Encoding judgement string.
currentEncodings :: IO String
currentEncodings =
    do locale  <- Enc.getLocaleEncoding
       file    <- Enc.getFileSystemEncoding
       return $ "|-- ENCODING  /content '" ++ show locale
                         ++ "  /file '"    ++ show file


-- --------------------------------------------  Exit

-- | Exit process.
exit :: Int -> IO a
exit = B.exitWith . exitCode

exitCode :: Int -> Exit.ExitCode
exitCode 0 = Exit.ExitSuccess
exitCode n = Exit.ExitFailure n

-- | Print message and exit on 0.
putSuccess :: String -> IO a
putSuccess msg = success $ putStr msg

putSuccessLn :: String -> IO a
putSuccessLn msg = success $ putStrLn msg

-- | Print error message and exit on 1.
putFailure :: String -> IO a
putFailure msg = failure $ IO.hPutStr IO.stderr msg

putFailureLn :: String -> IO a
putFailureLn msg = failure $ IO.hPutStrLn IO.stderr msg

success :: IO () -> IO a
success body = body >> B.exitWith B.ExitSuccess

failure :: IO () -> IO a
failure body = body >> B.exitWith (B.ExitFailure 1)


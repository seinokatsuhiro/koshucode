{-# OPTIONS_GHC -Wall #-}

-- | Exit process.

module Koshucode.Baala.Base.IO.Exit
  ( progAndArgs,
    useUtf8, currentEncodings,
    exit, putSuccess, putFailure,
  ) where

import qualified GHC.IO.Encoding               as Enc
import qualified System.Environment            as Sys
import qualified System.IO                     as IO
import qualified Koshucode.Baala.Base.Prelude  as B


-- | Program name and command-line arguments.
--   This function removes carrige returns from the arguments.
progAndArgs :: IO (String, [String])
progAndArgs =
    do useUtf8 B.stdout
       prog <- Sys.getProgName
       args <- Sys.getArgs
       return (prog, deleteCr args)

-- Delete carriage return from command line argument.
deleteCr :: [String] -> [String]
deleteCr = map (filter notCrChar) . filter notCrString where
    notCrChar   = (/= '\r')
    notCrString = (/= "\r")

useUtf8 :: IO.Handle -> IO ()
useUtf8 h = do B.setLocaleUtf8
               IO.hSetEncoding h IO.utf8

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
exit = B.exitWith . B.exitCode

-- | Print message and exit on 0.
putSuccess :: String -> IO a
putSuccess msg =
    do putStr msg
       B.exitWith B.ExitSuccess

-- | Print error message and exit on 1.
putFailure :: String -> IO a
putFailure msg =
    do IO.hPutStr IO.stderr msg
       B.exitWith $ B.ExitFailure 1


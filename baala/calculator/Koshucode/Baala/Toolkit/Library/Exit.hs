{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Toolkit.Library.Exit
  ( prelude,
    putSuccess, putFailure,
    currentEncodings,
    exit,
  ) where

import qualified GHC.IO.Encoding       as IO
import qualified System.Environment    as Sys
import qualified System.Exit           as Sys
import qualified System.IO             as IO
import qualified Koshucode.Baala.Base  as B

prelude :: IO (String, [String])
prelude =
    do B.useUtf8 B.stdout
       prog <- Sys.getProgName
       args <- Sys.getArgs
       return (prog, deleteCr args)

-- Delete carriage return from command line argument.
deleteCr :: [String] -> [String]
deleteCr = map (filter notCrChar) . filter notCrString where
    notCrChar   = (/= '\r')
    notCrString = (/= "\r")

-- | Print message and exit on 0.

putSuccess :: String -> IO a
putSuccess msg = do
  putStr msg
  Sys.exitWith $ Sys.ExitSuccess

-- | Print error message and exit on 1.

putFailure :: String -> IO a
putFailure msg = do
  IO.hPutStr IO.stderr msg
  Sys.exitWith $ Sys.ExitFailure 1

currentEncodings :: IO String
currentEncodings =
    do locale  <- IO.getLocaleEncoding
       file    <- IO.getFileSystemEncoding
       let q    = ("'" ++)
           j    = B.affirm "ENCODING" [ ("content", q $ show locale)
                                      , ("file",    q $ show file) ]
       return $ B.writeDownJudge B.shortEmpty j ++ "\n"

exit :: Int -> IO a
exit = Sys.exitWith . exitStatus

exitStatus :: Int -> Sys.ExitCode
exitStatus 0 = Sys.ExitSuccess
exitStatus n = Sys.ExitFailure n


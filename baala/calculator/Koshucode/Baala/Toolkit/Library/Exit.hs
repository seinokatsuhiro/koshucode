{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Toolkit.Library.Exit
  ( prelude,
    putSuccess, putFailure,
    currentEncodings,
    exit,
  ) where

import qualified GHC.IO.Encoding       as IO
import qualified System.Environment    as Sys
import qualified System.IO             as IO
import qualified Koshucode.Baala.Base  as B
import qualified Koshucode.Baala.Data  as D
import qualified Koshucode.Baala.Core  as C

prelude :: IO (String, [String])
prelude =
    do C.useUtf8 B.stdout
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
  B.exitWith $ B.ExitSuccess

-- | Print error message and exit on 1.

putFailure :: String -> IO a
putFailure msg = do
  IO.hPutStr IO.stderr msg
  B.exitWith $ B.ExitFailure 1

currentEncodings :: IO String
currentEncodings =
    do locale  <- IO.getLocaleEncoding
       file    <- IO.getFileSystemEncoding
       let q    = ("'" ++)
           j    = D.affirm "ENCODING" [ ("content", q $ show locale)
                                      , ("file",    q $ show file) ]
       return $ D.writeDownJudge D.shortEmpty j ++ "\n"

exit :: Int -> IO a
exit = B.exitWith . B.exitCode


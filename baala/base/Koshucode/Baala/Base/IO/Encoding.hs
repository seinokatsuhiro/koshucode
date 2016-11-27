{-# OPTIONS_GHC -Wall #-}

-- | Encoding.

module Koshucode.Baala.Base.IO.Encoding
  ( hSetKoshuOutput,
    currentEncodings,
    progAndArgs,
  ) where

import qualified GHC.IO.Encoding               as Enc
import qualified System.Environment            as Sys
import qualified System.IO                     as IO
import qualified Koshucode.Baala.Base.Prelude  as B

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
--
--   >>> currentEncodings
--   "|-- ENCODING  /content 'UTF-8  /file 'UTF-8"
--
currentEncodings :: IO String
currentEncodings =
    do locale  <- Enc.getLocaleEncoding
       file    <- Enc.getFileSystemEncoding
       return $ "|-- ENCODING  /content '" ++ show locale
                         ++ "  /file '"    ++ show file

-- | Program name and command-line arguments.
--   This function removes carrige returns from the arguments.
progAndArgs :: IO (String, [String])
progAndArgs =
    do hSetKoshuOutput B.stdout
       prog <- Sys.getProgName
       args <- Sys.getArgs
       return (prog, deleteCr args)

-- | Delete carriage return from command line argument.
deleteCr :: [String] -> [String]
deleteCr = map (filter notCrChar) . filter notCrString where
    notCrChar   = (/= '\r')
    notCrString = (/= "\r")


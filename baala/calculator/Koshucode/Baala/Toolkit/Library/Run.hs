{-# OPTIONS_GHC -Wall #-}

-- | Functions for implementing the Koshucode calculator.

module Koshucode.Baala.Toolkit.Library.Run
  ( runFiles,
    getProxies,
  ) where

import qualified System.Environment     as Env
import qualified Koshucode.Baala.Base   as B
import qualified Koshucode.Baala.Data   as D
import qualified Koshucode.Baala.Core   as C
import qualified Koshucode.Baala.Writer as W


-- | Read and union sections from files, and run the section.
runFiles :: (D.CContent c, W.ToJSON c) => C.Global c -> [B.IOPoint] -> IO B.ExitCode
runFiles g ns =
    do (abRes, _) <- C.resReadPoints g ns
       case abRes of
         Left a    -> C.globalAbort g a
         Right res -> case C.resRun res of
                        Left a   -> C.globalAbort g a
                        Right r  -> C.putResult r

-- | Get proxy settings.
--   This function looks environment variables
--   of @http_proxy@, @https_proxy@, and @ftp_proxy@.
getProxies :: IO [B.HttpProxy]
getProxies =
    do httpProxy   <- Env.lookupEnv "http_proxy"
       httpsProxy  <- Env.lookupEnv "https_proxy"
       ftpProxy    <- Env.lookupEnv "ftp_proxy"
       return [ ("http"  , httpProxy)
              , ("https" , httpsProxy)
              , ("ftp"   , ftpProxy) ]

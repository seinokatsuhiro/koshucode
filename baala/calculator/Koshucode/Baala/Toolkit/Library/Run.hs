{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Toolkit.Library.Run
  ( runFiles,
    getProxies,
    theContent,
    --mkdir,
  ) where

import qualified System.Environment     as Env
import qualified Koshucode.Baala.Base   as B
import qualified Koshucode.Baala.Data   as C
import qualified Koshucode.Baala.Core   as C
import qualified Koshucode.Baala.Writer as W



-- ----------------------

-- | Read and union sections from files, and run the section.
runFiles :: (C.CContent c, W.ToJSON c) => C.Global c -> [B.IOPoint] -> IO B.ExitCode
runFiles g ns =
    do (abRes, _) <- C.gioResource (C.readSources ns) g
       case abRes of
         Left a    -> C.globalAbort g a
         Right res -> case C.runResource res of
                        Left a   -> C.globalAbort g a
                        Right r  -> C.putResult r

getProxies :: IO [(String, Maybe String)]
getProxies =
    do httpProxy   <- Env.lookupEnv "http_proxy"
       httpsProxy  <- Env.lookupEnv "https_proxy"
       ftpProxy    <- Env.lookupEnv "ftp_proxy"
       return [ ("http"  , httpProxy)
              , ("https" , httpsProxy)
              , ("ftp"   , ftpProxy) ]



-- ---------------------- Calculation list

-- mkdir :: FilePath -> IO ()
-- mkdir path = 
--     do let dir = Path.dropFileName path
--        Dir.createDirectoryIfMissing True dir



-- ----------------------  The

theContent :: (C.CContent c) => String -> [B.Named c] -> Maybe c
theContent = lookup

-- theContents :: (C.CContent c) => [String] -> [B.Named c] -> Maybe [c]
-- theContents ns assn = mapM (`theContent` assn) ns

-- theStrings :: (C.CContent c) => c -> [String]
-- theStrings c | C.isText c  =  [C.gText c]
-- theStrings c | C.isList c  =  map C.gText $ C.gList c
-- theStrings _               =  []


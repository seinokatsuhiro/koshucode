{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Http
  ( uriContent,
    httpExceptionSummary,
  ) where

import qualified Data.ByteString.Char8           as Byte
import qualified Data.ByteString.Lazy.Char8      as Lazy
import qualified System.Environment              as Env
import qualified Control.Exception               as Ex
import qualified Network.HTTP.Conduit            as H
import qualified Network.HTTP.Types.Status       as H
import qualified Text.URI                        as H
import qualified Koshucode.Baala.Base.Prelude    as B


uriContent :: String -> IO (Either (Int, String) String)
uriContent uriText =
    do req <- requestFromURI uriText
       catchHttpException $ do
         res <- H.withManager $ H.httpLbs req
         return $ Right $ Lazy.unpack $ H.responseBody res

requestFromURI :: String -> IO H.Request
requestFromURI uriText =
    do req <- H.parseUrl uriText
       case H.parseURI uriText of
         Nothing  -> return req
         Just uri -> do proxy <- selectProxy uri
                        return $ add proxy req
    where 
      add :: Maybe String -> B.Map H.Request
      add proxy req = B.fromMaybe req $ do
          proxy'    <- proxy
          uri       <- H.parseURI proxy'
          host      <- H.uriRegName uri
          let host'  = Byte.pack host
              port   = B.fromMaybe 80 $ H.uriPort uri
              port'  = fromInteger port :: Int
          Just $ H.addProxy host' port' req

selectProxy :: H.URI -> IO (Maybe String)
selectProxy uri =
    case H.uriScheme uri of
      Just "http"  -> Env.lookupEnv "http_proxy"
      Just "https" -> Env.lookupEnv "https_proxy"
      Just "ftp"   -> Env.lookupEnv "ftp_proxy"
      _            -> return Nothing

catchHttpException :: B.Map (IO (Either (Int, String) String))
catchHttpException = ( `Ex.catch` handler ) where
    handler (H.StatusCodeException (H.Status code msg) _ _)
        = return $ Left (code, Byte.unpack msg)
    handler e = return $ Left (0, httpExceptionSummary e)

httpExceptionSummary :: H.HttpException -> String
httpExceptionSummary e = case e of
    H.StatusCodeException (H.Status _ m) _ _  -> Byte.unpack m
    H.InvalidUrlException _ _                 -> "Invalid URL"
    H.TooManyRedirects    _                   -> "Too many redirects"
    H.UnparseableRedirect _                   -> "Unparseable redirect"
    H.TooManyRetries                          -> "Too many retries"
    H.HttpParserException _                   -> "Could not parse HTTP message"
    H.HandshakeFailed                         -> "Handshake failed"
    H.OverlongHeaders                         -> "Overlong headers"
    H.ResponseTimeout                         -> "Response timeout"
    H.FailedConnectionException  _ _          -> "Connection failed"
    H.FailedConnectionException2 _ _ _ _      -> "Connection failed"
    H.ExpectedBlankAfter100Continue           -> "Expected blank after 100"
    H.InvalidStatusLine _                     -> "Invalid status line"
    H.InvalidHeader _                         -> "Invalid header"
    H.ProxyConnectException _ _ _             -> "Proxy connection error"
    H.NoResponseDataReceived                  -> "No response data received"
    H.TlsException _                          -> "TLS error"
    H.TlsNotSupported                         -> "TLS not supported"
    H.ResponseBodyTooShort _ _                -> "Response body too short"
    H.InvalidChunkHeaders                     -> "Invalid chunk headers"
    H.IncompleteHeaders                       -> "Incomplete headers"
    H.InvalidDestinationHost _                -> "Invalid destination host"
    H.HttpZlibException _                     -> "Zlib error"
    H.InternalIOException _                   -> "Internal I/O error"


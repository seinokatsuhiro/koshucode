{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Http
  ( UriText, HttpProxy,
    uriContent,
    httpExceptionSummary,
  ) where

import qualified Data.ByteString.Char8              as Byte
import qualified Data.ByteString.Lazy.UTF8          as UTF
import qualified Control.Exception                  as Ex
import qualified Network.HTTP.Conduit               as H
import qualified Network.HTTP.Types.Status          as H
import qualified Network.URI                        as URI
import qualified Koshucode.Baala.Base.Prelude       as B
import qualified Koshucode.Baala.Base.Text.Utility  as B

type UriText = String

-- | Pair of protocol name and proxy URI.
type HttpProxy = (String, Maybe UriText)

uriContent :: [HttpProxy] -> UriText -> IO (Either (Int, String) String)
uriContent proxies uriText =
    do req <- requestFromURI proxies uriText
       catchHttpException $ do
         man <- H.newManager H.tlsManagerSettings
         res <- H.httpLbs req man
         return $ Right $ UTF.toString $ H.responseBody res

requestFromURI :: [HttpProxy] -> UriText -> IO H.Request
requestFromURI proxies uriText =
    do req <- H.parseUrl uriText
       case URI.parseURI uriText of
         Nothing  -> return req
         Just uri -> do let proxy = selectProxy proxies uri
                        return $ add proxy req
    where 
      add :: Maybe UriText -> B.Map H.Request
      add proxy req = B.fromMaybe req $ do
          proxy'    <- proxy
          uri       <- URI.parseURI proxy'
          auth      <- URI.uriAuthority uri
          let host'  = Byte.pack $ URI.uriRegName auth
              port   = uriPortNumber $ URI.uriPort auth
          Just $ H.addProxy host' port req

uriPortNumber :: String -> Int
uriPortNumber (':' : n) = B.fromMaybe defaultPortNumber $ B.readInt n
uriPortNumber _         = defaultPortNumber

defaultPortNumber :: Int
defaultPortNumber = 80

selectProxy :: [HttpProxy] -> URI.URI -> Maybe UriText
selectProxy proxies uri =
    do let scheme = URI.uriScheme uri
       proxy <- lookup scheme proxies
       proxy

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
    H.InvalidProxyEnvironmentVariable _ _     -> "Invalid proxy env"
    H.ResponseLengthAndChunkingBothUsed       -> "Length and chunked"


{-# OPTIONS_GHC -Wall #-}

-- | Retrieve via HTTP.
module Koshucode.Baala.Base.IO.Http
  ( UriText, HttpProxy,
    uriContent,
    httpExceptionSummary,
  ) where

import qualified Data.ByteString.Char8              as Bc
import qualified Control.Exception                  as Ex
import qualified Network.HTTP.Conduit               as H
import qualified Network.HTTP.Types.Status          as H
import qualified Network.URI                        as URI
import qualified Koshucode.Baala.Overture           as O
import qualified Koshucode.Baala.Base.Prelude       as B

-- | URI.
type UriText = String

-- | Pair of protocol name and proxy URI.
type HttpProxy = (String, Maybe UriText)

-- | Get HTTP content as lazy bytestring.
uriContent :: [HttpProxy] -> UriText -> IO (Either (Int, String) B.Bz)
uriContent proxies uriText =
    do req <- requestFromURI proxies uriText
       catchHttpException $ do
         man <- H.newManager H.tlsManagerSettings
         res <- H.httpLbs req man
         return $ Right $ H.responseBody res

requestFromURI :: [HttpProxy] -> UriText -> IO H.Request
requestFromURI proxies uriText =
    do req <- H.parseRequest uriText
       case URI.parseURI uriText of
         Nothing  -> return req
         Just uri -> do let proxy = selectProxy proxies uri
                        return $ add proxy req
    where 
      add :: Maybe UriText -> O.Map H.Request
      add proxy req = B.fromMaybe req $ do
          proxy'    <- proxy
          uri       <- URI.parseURI proxy'
          auth      <- URI.uriAuthority uri
          let host'  = Bc.pack $ URI.uriRegName auth
              port   = uriPortNumber $ URI.uriPort auth
          Just $ H.addProxy host' port req

uriPortNumber :: String -> Int
uriPortNumber (':' : n) = B.fromMaybe defaultPortNumber $ O.readInt n
uriPortNumber _         = defaultPortNumber

defaultPortNumber :: Int
defaultPortNumber = 80

selectProxy :: [HttpProxy] -> URI.URI -> Maybe UriText
selectProxy proxies uri =
    do let scheme = URI.uriScheme uri
       proxy <- lookup scheme proxies
       proxy

catchHttpException :: O.Map (IO (Either (Int, String) B.Bz))
catchHttpException = ( `Ex.catch` handler ) where
    handler (H.HttpExceptionRequest _ (H.StatusCodeException res _))
              = return $ Left (httpStatus res)
    handler e = return $ Left (0, httpExceptionSummary e)

-- | Status code and message.
httpStatus :: H.Response body -> (Int, String)
httpStatus res = case H.responseStatus res of
                   H.Status code msg -> (code, Bc.unpack msg)

-- | Text message of HTTP exception.
httpExceptionSummary :: H.HttpException -> String
httpExceptionSummary (H.InvalidUrlException _ _)  = "Invalid URL"
httpExceptionSummary (H.HttpExceptionRequest _ e) = httpExceptionContent e

httpExceptionContent :: H.HttpExceptionContent -> String
httpExceptionContent e = case e of
    H.StatusCodeException res _             -> snd $ httpStatus res
    H.TooManyRedirects    _                 -> "Too many redirects"
    H.OverlongHeaders                       -> "Overlong headers"
    H.ResponseTimeout                       -> "Response timeout"
    H.ConnectionTimeout                     -> "Connection timeout"
    H.ConnectionFailure _                   -> "Connection failure"
    H.InvalidStatusLine _                   -> "Invalid status line"
    H.InvalidHeader _                       -> "Invalid header"
    H.InternalException _                   -> "Internal exception"
    H.ProxyConnectException _ _ _           -> "Proxy connection error"
    H.NoResponseDataReceived                -> "No response data received"
    H.TlsNotSupported                       -> "TLS not supported"
    H.WrongRequestBodyStreamSize _ _        -> "Wrong request body"
    H.ResponseBodyTooShort _ _              -> "Response body too short"
    H.InvalidChunkHeaders                   -> "Invalid chunk headers"
    H.IncompleteHeaders                     -> "Incomplete headers"
    H.InvalidDestinationHost _              -> "Invalid destination host"
    H.HttpZlibException _                   -> "Zlib error"
    H.InvalidProxyEnvironmentVariable _ _   -> "Invalid proxy env"
    H.ConnectionClosed                      -> "Closed"


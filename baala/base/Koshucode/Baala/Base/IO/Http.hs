{-# OPTIONS_GHC -Wall #-}

-- | Retrieve via HTTP.
module Koshucode.Baala.Base.IO.Http
  ( HttpProxy,
    httpGet,
    httpExceptionSummary,
  ) where

import qualified Control.Exception                  as Ex
import qualified Network.HTTP.Conduit               as H
import qualified Network.HTTP.Types.Status          as H
import qualified Network.URI                        as U
import qualified Koshucode.Baala.Overture           as O
import qualified Koshucode.Baala.Base.Prelude       as B

-- | Pair of protocol name and proxy URI.
type HttpProxy = (String, Maybe O.IOPath)

-- | Get HTTP content as lazy bytestring.
--   If request succeeded as 200 OK,
--   returns lazy-bytestring content in 'Right' part,
--   otherwise, returns status code and message in 'Left' part.
--
--   >>> httpGet [] "https://httpbin.org/robots.txt"
--   Right "User-agent: *\nDisallow: /deny\n"
--
--   >>> (print B.<#.> httpGet []) O.<#!> ((\c -> "http://httpbin.org/status/" ++ show c) <$> [400 .. 404 :: Int])
--   Left (400, "BAD REQUEST")
--   Left (401, "UNAUTHORIZED")
--   Left (402, "PAYMENT REQUIRED")
--   Left (403, "FORBIDDEN")
--   Left (404, "NOT FOUND")
--
httpGet :: [HttpProxy] -> O.IOPath -> IO (Either (Int, String) B.Bz)
httpGet proxies uriText =
    do req <- requestFromURI proxies uriText
       catchHttpException $ do
         man <- H.newManager H.tlsManagerSettings
         res <- H.httpLbs req man
         return $ case H.responseStatus res of
           H.Status code msg
               | code == 200 -> Right $ H.responseBody res
               | otherwise   -> Left (code, B.bsString msg)

requestFromURI :: [HttpProxy] -> O.IOPath -> IO H.Request
requestFromURI proxies uriText =
    do req <- H.parseRequest uriText
       case U.parseURI uriText of
         Nothing  -> return req
         Just uri -> do let proxy = selectProxy proxies uri
                        return $ add proxy req
    where 
      add :: Maybe O.IOPath -> O.Map H.Request
      add proxy req = B.fromMaybe req $ do
          proxy'    <- proxy
          uri       <- U.parseURI proxy'
          auth      <- U.uriAuthority uri
          let host'  = B.stringBs $ U.uriRegName auth
              port   = uriPortNumber $ U.uriPort auth
          Just $ H.addProxy host' port req

uriPortNumber :: String -> Int
uriPortNumber (':' : n) = B.fromMaybe defaultPortNumber $ O.stringInt n
uriPortNumber _         = defaultPortNumber

defaultPortNumber :: Int
defaultPortNumber = 80

selectProxy :: [HttpProxy] -> U.URI -> Maybe O.IOPath
selectProxy proxies uri =
    do let scheme = U.uriScheme uri
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
                   H.Status code msg -> (code, B.bsString msg)

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


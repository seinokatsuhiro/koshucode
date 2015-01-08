{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Source
  ( Source (..), SourceName (..),
    sourceType, sourceText,
    sourceZero, sourceOf, sourceList,
    uriContent,
  ) where

import qualified Data.Generics                   as G
import qualified Data.ByteString.Lazy.Char8      as Lazy
import qualified Data.ByteString.Char8           as Byte
import qualified System.Environment              as Env
import qualified Text.URI                        as HTTP
import qualified Control.Exception               as HTTP
import qualified Network.HTTP.Types.Status       as HTTP
import qualified Network.HTTP.Conduit            as HTTP
import qualified Koshucode.Baala.Base.Prelude    as B

data Source
    = Source { sourceNumber :: Int
             , sourceName   :: SourceName }
      deriving (Show, G.Data, G.Typeable)

instance Eq Source where
    x == y = sourceName x == sourceName y

instance Ord Source where
    x `compare` y = sourceName x `compare` sourceName y

data SourceName
    = SourceFile  FilePath
    | SourceURL   String
    | SourceText  String
    | SourceStdin
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Name of resourcd type, i.e., @\"file\"@, @\"text\"@, @\"url\"@.
sourceType :: Source -> String
sourceType = sourceNameType . sourceName

sourceText :: Source -> String
sourceText = sourceNameText . sourceName

sourceNameType :: SourceName -> String
sourceNameType (SourceFile _)     = "file"
sourceNameType (SourceURL  _)     = "url"
sourceNameType (SourceText _)     = "text"
sourceNameType (SourceStdin)      = "stdin"

sourceNameText :: SourceName -> String
sourceNameText (SourceFile file)  = file
sourceNameText (SourceURL  url)   = url
sourceNameText (SourceText text)  = text
sourceNameText (SourceStdin)      = "<stdin>"

-- | Empty source.
sourceZero :: Source
sourceZero = sourceOf ""

-- | Create text source.
sourceOf :: String -> Source
sourceOf = Source 0 . SourceText

-- | Create sources from using stdin, texts itself, filenames, and urls.
sourceList :: Bool -> [String] -> [String] -> [String] -> [Source]
sourceList stdin texts files urls = zipWith Source [1..] names where
    input = if stdin then [SourceStdin] else []
    names = input ++
            SourceText `map` texts ++
            SourceFile `map` files ++
            SourceURL  `map` urls

uriContent :: String -> IO (Either (Int, String) String)
uriContent uriText =
    do req <- requestFromURI uriText
       catchHttpException $ do
         res <- HTTP.withManager $ HTTP.httpLbs req
         return $ Right $ Lazy.unpack $ HTTP.responseBody res

requestFromURI :: String -> IO HTTP.Request
requestFromURI uriText =
    do req <- HTTP.parseUrl uriText
       case HTTP.parseURI uriText of
         Nothing  -> return req
         Just uri -> do proxy <- selectProxy uri
                        return $ maybe req (proxy1 req) proxy
    where
      proxy1 req u    = maybe req (proxy2 req) $ HTTP.parseURI u
      proxy2 req u    = maybe req (proxy3 req u) $ HTTP.uriRegName u
      proxy3 req u h  = let host   = Byte.pack h
                            port   = B.fromMaybe 80 $ HTTP.uriPort u
                            port'  = (fromInteger port) :: Int
                        in HTTP.addProxy host port' req

selectProxy :: HTTP.URI -> IO (Maybe String)
selectProxy uri =
    case HTTP.uriScheme uri of
      Just "http"  -> Env.lookupEnv "http_proxy"
      Just "https" -> Env.lookupEnv "https_proxy"
      Just "ftp"   -> Env.lookupEnv "ftp_proxy"
      _            -> return Nothing

catchHttpException :: B.Map (IO (Either (Int, String) String))
catchHttpException = ( `HTTP.catch` handler ) where
    handler (HTTP.StatusCodeException (HTTP.Status code msg) _ _)
        = return $ Left (code, Byte.unpack msg)
    handler e = error $ show e


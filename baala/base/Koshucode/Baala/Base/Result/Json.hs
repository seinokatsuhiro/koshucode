{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Result.Json
  ( -- * JSON
    resultJson,
    termsToJSON,
    jsonNull,
    A.ToJSON (..),

    -- * GeoJSON
    resultGeoJson,
    ToGeoJSON (..),
  ) where

import           Data.Aeson                        ((.=))
import qualified Data.Aeson                        as A
import qualified Data.ByteString.Lazy              as Byte
import qualified Data.Text                         as T
import qualified System.IO                         as IO
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Judge   as B
import qualified Koshucode.Baala.Base.Result.Result as B


-- --------------------------------------------  JSON

resultJson :: (A.ToJSON c) => B.ResultWriter c
resultJson = B.ResultWriterJudge "json" hPutJSON

hPutJSON :: (A.ToJSON c) => B.ResultWriterJudge c
hPutJSON _ _ status [] = return status
hPutJSON h _ status (j1:js) =
    do IO.hPutStr h "[ "
       put j1
       mapM_ cput js
       IO.hPutStrLn h "]"
       return status
    where
      put j  = do Byte.hPutStr h $ A.encode j
                  IO.hPutChar h '\n'
      cput j = do IO.hPutStr h ", "
                  put j

instance (A.ToJSON c) => A.ToJSON (B.Judge c) where
    toJSON (B.JudgeAffirm p xs) =
        A.object [ "judge" .= text "|--"
                 , "name"  .= p
                 , "args"  .= termsToJSON xs ]
    toJSON _ = undefined

termsToJSON :: (A.ToJSON c) => [B.Term c] -> A.Value
termsToJSON xs = A.object $ map json xs where
    json (n, c) = (T.pack n, A.toJSON c)

text :: T.Text -> T.Text
text s = s

jsonNull :: A.Value
jsonNull = A.Null


-- --------------------------------------------  GeoJSON

resultGeoJson :: (A.ToJSON c) => B.ResultWriter c
resultGeoJson = B.ResultWriterJudge "geojson" hPutGeoJson

hPutGeoJson :: (A.ToJSON c) => B.ResultWriterJudge c
hPutGeoJson _ _ status [] = return status
hPutGeoJson h _ status (j1:js) =
    do IO.hPutStrLn h "{ \"type\": \"FeatureCollection\""
       IO.hPutStrLn h ", \"crs\": {\"type\": \"name\", \"properties\": {\"name\": \"urn:ogc:def:crs:OGC:1.3:CRS84\"}}"
       IO.hPutStrLn h ", \"features\": ["
       IO.hPutStr   h "  "
       put j1
       mapM_ cput js
       IO.hPutStrLn h "]}"
       return status
    where
      put j  = do Byte.hPutStr h $ A.encode $ toGeoJSON j
                  IO.hPutChar h '\n'
      cput j = do IO.hPutStr h ", "
                  put j

class ToGeoJSON a where
    toGeoJSON :: a -> A.Value

instance (A.ToJSON c) => ToGeoJSON (B.Judge c) where
    toGeoJSON (B.JudgeAffirm _ xs) =
        A.object [ "type"       .= text "Feature"
                 , "properties" .= A.object [ "name" .= name ]
                 , "geometry"   .= geo ]
            where name   = lookup "name" xs
                  lat    = lookup "lat"  xs
                  long   = lookup "long" xs
                  geo    = A.object [ "type"        .= text "Point"
                                    , "coordinates" .= [long, lat]]
    toGeoJSON _ = undefined


{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Koshucode.Baala.Writer.Json
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
import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Core              as C


-- --------------------------------------------  JSON

resultJson :: (A.ToJSON c) => C.ResultWriter c
resultJson = C.ResultWriterJudge "json" hPutJSON

hPutJSON :: (A.ToJSON c) => C.ResultWriterJudge c
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

instance (A.ToJSON c) => A.ToJSON (D.Judge c) where
    toJSON (D.JudgeAffirm p xs) =
        A.object [ "judge" .= text "|--"
                 , "name"  .= p
                 , "args"  .= termsToJSON xs ]
    toJSON _ = undefined

termsToJSON :: (A.ToJSON c) => [D.Term c] -> A.Value
termsToJSON xs = A.object $ map json xs where
    json (n, c) = (T.pack n, A.toJSON c)

text :: T.Text -> T.Text
text s = s

jsonNull :: A.Value
jsonNull = A.Null

instance A.ToJSON D.BaalaC where
    toJSON c = case c of
        D.VText s      -> A.toJSON s
        D.VTerm s      -> A.toJSON $ '/' : s
        D.VDec  n      -> A.toJSON (D.decimalToRealFloat n :: Double)
        D.VClock t     -> unimplemented t
        D.VTime t      -> unimplemented t
        D.VBool b      -> A.toJSON b
        D.VEmpty       -> jsonNull
        D.VInterp i    -> unimplemented i
        D.VType t      -> unimplemented t
        D.VList xs     -> A.toJSON xs
        D.VSet  xs     -> A.toJSON xs
        D.VAssn xs     -> termsToJSON xs
        D.VRel r       -> unimplemented r
        where unimplemented x = A.toJSON $ "<unimplemented>" ++ show x


-- --------------------------------------------  GeoJSON

resultGeoJson :: (A.ToJSON c) => C.ResultWriter c
resultGeoJson = C.ResultWriterJudge "geojson" hPutGeoJson

hPutGeoJson :: (A.ToJSON c) => C.ResultWriterJudge c
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

instance (A.ToJSON c) => ToGeoJSON (D.Judge c) where
    toGeoJSON (D.JudgeAffirm _ xs) =
        A.object [ "type"       .= text "Feature"
                 , "properties" .= A.object [ "name" .= name ]
                 , "geometry"   .= geo ]
            where name   = lookup "name" xs
                  lat    = lookup "lat"  xs
                  long   = lookup "long" xs
                  geo    = A.object [ "type"        .= text "Point"
                                    , "coordinates" .= [long, lat]]
    toGeoJSON _ = undefined


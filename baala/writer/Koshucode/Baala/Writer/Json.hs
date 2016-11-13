{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | JSON output.

module Koshucode.Baala.Writer.Json
  ( -- * JSON
    resultJson,
    termsJson,
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
import qualified Koshucode.Baala.Syntax            as S
import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Core              as C


-- --------------------------------------------  JSON

-- | JSON writer.
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
        A.object [ "judge" .= ("|--" :: T.Text)
                 , "name"  .= p
                 , "args"  .= termsJson xs ]
    toJSON _ = undefined

-- | Encode term list into JSON.
termsJson :: (A.ToJSON c) => [S.Term c] -> A.Value
termsJson xs = A.object $ map json xs where
    json (n, c) = (T.pack n, A.toJSON c)

-- | JSON null.
jsonNull :: A.Value
jsonNull = A.Null

-- | Convert content to JSON.
contentJson :: (A.ToJSON c, D.CContent c) => c -> A.Value
contentJson c
    | D.isCode    c = A.toJSON    $ D.gCode c
    | D.isText    c = A.toJSON    $ D.gText c
    | D.isTerm    c = A.toJSON    $ S.showTermName $ D.gTerm c
    | D.isDec     c = A.toJSON    $ D.decimalDouble $ D.gDec c
    | D.isClock   c = unimpl      $ D.gClock c
    | D.isTime    c = unimpl      $ D.gTime c
    | D.isBool    c = A.toJSON    $ D.gBool c
    | D.isEmpty   c = jsonNull
    | D.isEnd     c = jsonNull
    | D.isList    c = A.toJSON    $ D.gList c
    | D.isSet     c = A.toJSON    $ D.gSet c
    | D.isTie     c = termsJson   $ D.gTie c
    | D.isRel     c = unimpl      $ D.gRel c
    | D.isInterp  c = unimpl      $ D.gInterp c
    | D.isType    c = unimpl      $ D.gType c
    | otherwise     = unimpl c
    where unimpl  x = A.toJSON $ "<unimplemented>" ++ show x

instance A.ToJSON D.Content where
    toJSON = contentJson


-- --------------------------------------------  GeoJSON

-- | GeoJSON writer.
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

-- | Convert to GeoJSON.
class ToGeoJSON a where
    toGeoJSON :: a -> A.Value

instance (A.ToJSON c) => ToGeoJSON (D.Judge c) where
    toGeoJSON (D.JudgeAffirm _ xs) =
        A.object [ "type"       .= ("Feature" :: T.Text)
                 , "properties" .= A.object [ "name" .= name ]
                 , "geometry"   .= geo ]
            where name   = lookup "name" xs
                  lat    = lookup "lat"  xs
                  long   = lookup "long" xs
                  geo    = A.object [ "type"        .= ("Point" :: T.Text)
                                    , "coordinates" .= [long, lat]]
    toGeoJSON _ = undefined


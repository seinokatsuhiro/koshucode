{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Writer.Html
  ( resultHtmlIndented, resultHtmlCompact,
  ) where

import qualified System.IO                          as IO
import qualified Text.Blaze.XHtml5                  as H
import           Text.Blaze.XHtml5                  ((!))
import           Text.Blaze.XHtml5.Attributes       (class_)
import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Syntax             as D
import qualified Koshucode.Baala.Core               as C

resultHtmlIndented :: (B.Write c) => C.ResultWriter c
resultHtmlIndented = C.ResultWriterChunk "html-indented" (hPutHtml B.renderHtmlIndented)

resultHtmlCompact :: (B.Write c) => C.ResultWriter c
resultHtmlCompact  = C.ResultWriterChunk "html-compact"  (hPutHtml B.renderHtmlCompact)

hPutHtml :: (B.Write c) => (H.Html -> String) -> C.ResultWriterChunk c
hPutHtml render h _ status sh =
    do hPutRel h render sh
       return status

hPutRel :: (B.Write c) => IO.Handle -> (H.Html -> String) -> [C.ShortResultChunks c] -> IO ()
hPutRel h render sh = mapM_ put chunks where
    chunks = concatMap D.shortBody sh
    put (C.ResultRel pat r) = IO.hPutStrLn h $ render $ html pat r
    put _                 = return ()
    html pat r = H.div ! class_ "named-relation" $ do
                   H.p ! class_ "name" $ H.toHtml pat
                   B.writeHtmlWith B.nullShortener r


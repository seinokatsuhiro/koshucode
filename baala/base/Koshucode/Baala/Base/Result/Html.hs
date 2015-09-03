{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Result.Html
  ( resultHtmlIndented, resultHtmlCompact,
  ) where

import qualified System.IO                          as IO
import qualified Text.Blaze.XHtml5                  as H
import           Text.Blaze.XHtml5                  ((!))
import           Text.Blaze.XHtml5.Attributes       (class_)
import qualified Koshucode.Baala.Base.Text          as B
import qualified Koshucode.Baala.Base.Token         as B
import qualified Koshucode.Baala.Base.Result.Result as B


resultHtmlIndented :: (B.Write c) => B.ResultWriter c
resultHtmlIndented = B.ResultWriterChunk "html-indented" (hPutHtml B.renderHtmlIndented)

resultHtmlCompact :: (B.Write c) => B.ResultWriter c
resultHtmlCompact  = B.ResultWriterChunk "html-compact"  (hPutHtml B.renderHtmlCompact)

hPutHtml :: (B.Write c) => (H.Html -> String) -> B.ResultWriterChunk c
hPutHtml render h _ status sh =
    do hPutRel h render sh
       return status

hPutRel :: (B.Write c) => IO.Handle -> (H.Html -> String) -> [B.ShortResultChunks c] -> IO ()
hPutRel h render sh = mapM_ put chunks where
    chunks = concatMap B.shortBody sh
    put (B.ResultRel pat r) = IO.hPutStrLn h $ render $ html pat r
    put _                 = return ()
    html pat r = H.div ! class_ "named-relation" $ do
                   H.p ! class_ "name" $ H.toHtml pat
                   B.writeHtmlWith id r


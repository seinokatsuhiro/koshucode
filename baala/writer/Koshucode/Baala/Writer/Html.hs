{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Writer.Html
  ( resultHtmlIndented,
    resultHtmlCompact,
    contToHtml,
  ) where

import qualified System.IO                          as IO
import qualified Text.Blaze.XHtml5                  as H
import           Text.Blaze.XHtml5                  ((!))
import           Text.Blaze.XHtml5.Attributes       (class_)
import qualified Text.Blaze.Html.Renderer.Pretty    as Hi
import qualified Text.Blaze.Html.Renderer.String    as Hc
import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Syntax             as S
import qualified Koshucode.Baala.Core               as C

resultHtmlIndented :: (D.CContent c) => C.ResultWriter c
resultHtmlIndented = C.ResultWriterChunk "html-indented" (hPutHtml Hi.renderHtml)

resultHtmlCompact :: (D.CContent c) => C.ResultWriter c
resultHtmlCompact  = C.ResultWriterChunk "html-compact"  (hPutHtml Hc.renderHtml)

hPutHtml :: (D.CContent c) => (H.Html -> String) -> C.ResultWriterChunk c
hPutHtml render h _ status sh =
    do hPutRel h render sh
       return status

hPutRel :: (D.CContent c) => IO.Handle -> (H.Html -> String) -> [C.ShortResultChunks c] -> IO ()
hPutRel h render sh = mapM_ put chunks where
    chunks = concatMap S.shortBody sh
    put (C.ResultRel cl r) = IO.hPutStrLn h $ render $ html cl r
    put _                  = return ()
    html cl r = H.div ! class_ "named-relation" $ do
                  H.p ! class_ "name" $ H.toHtml cl
                  contToHtml B.noShorten $ D.pRel r

-- | Encode term content in HTML.
contToHtml :: (D.CContent c) => B.Shorten -> c -> H.Html
contToHtml sh = content where
    content c
        | D.isRel c = rel $ D.gRel c
        | otherwise = H.toHtml $ B.mixToFlatString $ B.mixShortEncode sh c

    rel (D.Rel he bo) =
        H.table ! class_ "relation" $ do
          let terms = term `map` D.headNames he
          H.tr ! class_ "heading" $ mapM_ H.td terms
          mapM_ row bo

    row cs = H.tr ! class_ "tuple" $ mapM_ col cs
    col c  = H.td $ contToHtml sh c
    term   = (H.span ! class_ "termname") . H.toHtml . S.showTermName


{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | HTML output.

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
import qualified Koshucode.Baala.Overture           as O
import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Syntax             as S
import qualified Koshucode.Baala.Core               as C

-- | HTML output in indent format.
resultHtmlIndented :: (D.CContent c) => C.ResultWriter c
resultHtmlIndented = C.ResultWriterChunk "html-indented" (hPutHtml Hi.renderHtml)

-- | HTML output in linear format.
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
                  H.p ! class_ "name" $ H.toMarkup cl
                  contToHtml O.nothing $ D.pRel r

-- | Encode term content in HTML.
contToHtml :: (D.CContent c) => B.TransString -> c -> H.Html
contToHtml sh = content where
    content c
        | D.isRel c = rel $ D.gRel c
        | otherwise = H.toMarkup $ B.mixToFlatString $ B.mixTransEncode sh c

    rel (D.Rel he bo) =
        H.table ! class_ "relation" $ do
          let terms = term `map` D.getTermNames he
          H.tr ! class_ "heading" $ mapM_ H.td terms
          mapM_ row bo

    row cs = H.tr ! class_ "tuple" $ mapM_ col cs
    col c  = H.td $ contToHtml sh c
    term   = (H.span ! class_ "termname") . H.toMarkup . S.termNameString

--  HTML
--
--    Aborted
--
--    {main reason}
--      {detailed reason}
--      {detailed reason}
--
--    {line #} {col #} {filename}
--      {line text}
--    {line #} {col #} {filename}
--      {line text}

-- | Encode abort reason in HTML.
instance H.ToMarkup B.AbortReason where
    toMarkup a =
        div_ "abort" $ do
          div_ "abort-title"   $ text "Aborted"
          div_ "abort-reason"  $ text $ B.abortReason a
          div_ "abort-detail"  $ mapM_ detail  $ B.abortDetail a
          div_ "abort-source"  $ mapM_ code $ B.abortPoint a
        where
          text n          = H.toMarkup (n :: String)
          detail x        = H.div $ H.toMarkup x
          code (cp, tag)  = point cp tag >> line cp
          line p          = div_ "abort-line" $ text $ B.cpText p
          source k n c    = span_ k $ text n >> (H.toMarkup c)
          point cp tag    = div_ "abort-point" $ do
                                  source "abort-point-line"    "Line "    $ B.cpLineNo cp
                                  source "abort-point-column"  "Column "  $ B.cpColumnNo cp
                                  source "abort-point-context" "Context " tag

-- | 'H.div' with class name.
div_ :: H.AttributeValue -> O.Map H.Html
div_ c = H.div H.! class_ c

-- | 'H.span' with class name.
span_ :: H.AttributeValue -> O.Map H.Html
span_ c = H.span H.! class_ c


{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | HTML output.
--
-- Relations ('D.Rel') are formatted like:
--
--   > <div class="named-relation">
--   >   <p class="name">JUDGEMENT-CLASS
--   >   <table class="relation">
--   >     <tr class="heading">
--   >       <td class="term-name"><span class="term-slash">/</span>TERM-NAME
--   >       <td class="term-name"><span class="term-slash">/</span>TERM-NAME
--   >       ...
--   >     <tr class="tuple">
--   >       <td>CONTENT
--   >       <td>CONTENT
--   >       ...
--   >     <tr class="tuple">
--   >       <td>CONTENT
--   >       <td>CONTENT
--   >       ...
--   >     ...
--
-- Abort reasons ('B.AbortReason') are formatted like:
--
--   > <div class="abort">
--   >   <div class="abort-title">Aborted
--   >   <div class="abort-reason">MAIN-REASON
--   >   <div class="abort-detail">
--   >     <div>DETAILED-REASON
--   >     <div>DETAILED-REASON
--   >     ...
--   >
--   >   <div class="abort-source">
--   >     <div class="abort-pos">
--   >       <div class="abort-index">
--   >         <span class="abort-line">LINE
--   >         <span class="abort-char">CHAR
--   >         <span class="abort-path">PATH
--   >       <pre class="abort-before">BEFORE-CODE
--   >       <pre class="abort-after">AFTER-CODE
--   >
--   >     <div class="abort-pos">
--   >       <div class="abort-index">
--   >         <span class="abort-line">LINE
--   >         <span class="abort-char">CHAR
--   >         <span class="abort-path">PATH
--   >       <pre class="abort-before">BEFORE-CODE
--   >       <pre class="abort-after">AFTER-CODE
--   >     ...
--   >
--   >   <div class="abort-note">
--   >     <pre>NOTE-LINE
--   >     <pre>NOTE-LINE
--   >     ...
--
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
import qualified Koshucode.Baala.Type               as T
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
hPutHtml render h _ _ sh = hPutRel h render sh

hPutRel :: (D.CContent c) => IO.Handle -> (H.Html -> String) -> [C.ShortResultChunks c] -> IO ()
hPutRel h render sh = put O.<#!> chunks where
    chunks = concatMap S.shortBody sh
    put (C.ResultRel cl r) = IO.hPutStrLn h $ render $ html cl r
    put _                  = return ()
    html cl r = div_ "named-relation" $ do
                  H.p ! class_ "name" $ H.toMarkup cl
                  contToHtml O.nothing $ D.pRel r

-- | Encode term content in HTML.
contToHtml :: (D.CContent c) => B.TransText String -> c -> H.Html
contToHtml sh = content where
    content c
        | D.isRel c = rel $ D.gRel c
        | otherwise = H.toMarkup $ B.mixToFlatString $ B.mixTransEncode sh c

    rel (T.Rel he bo) =
        H.table ! class_ "relation" $ do
          tr_ "heading" (term O.<#!> T.getTermNames he)
          row O.<#!> bo

    row cs = tr_ "tuple" (col O.<#!> cs)
    col c  = H.td $ contToHtml sh c
    term t = td_ "term-name" $ do
               span_ "term-slash" $ H.toMarkup ("/" :: String)
               H.toMarkup $ S.termNameContent t

-- | Encode abort reason in HTML.
--
--   >>> let cp = B.CodePos 0 "<stdin>" 1 "abcdefg" "defg"
--   >>> let Left a = B.abortable "tag" cp $ Left $ B.abortBecause "Bad luck"
--   >>> putStrLn $ Hi.renderHtml $ H.toMarkup a
--
instance H.ToMarkup B.AbortReason where
    toMarkup a =
        div_ "abort" $ do
          div_ "abort-title"   (text "Aborted")
          div_ "abort-reason"  (text $ B.abortReason a)
          div_ "abort-detail"  ((div' . dash) O.<#!> B.abortDetail a)
          div_ "abort-source"  (loc    O.<#!> B.abortPointUp a)
          div_ "abort-note"    (pre'   O.<#!> B.abortNote a)
        where
          text s          = H.toMarkup (s :: String)
          div'            = H.div . H.toMarkup
          pre'            = H.pre . H.toMarkup

          dash (' ' : s)  = "– " ++ dash s
          dash s          = s

          loc (cp, _)     = pos cp
          source k c      = span_ k (H.toMarkup c)
          pos cp          = case B.cpMessage cp of
                              (Just (lno, cno, path), before, Just after)
                                  -> div_ "abort-pos" $ do
                                       div_ "abort-index" $ do
                                         source "abort-line" lno
                                         source "abort-char" cno
                                         source "abort-path" path
                                       case before of
                                         Nothing   -> return ()
                                         Just code -> pre_ "abort-before" $ text code
                                       pre_ "abort-after" $ text after
                              _ -> return ()

-- | 'H.div' with class name.
div_ :: H.AttributeValue -> O.Map H.Html
div_ c = H.div H.! class_ c

-- | 'H.span' with class name.
span_ :: H.AttributeValue -> O.Map H.Html
span_ c = H.span H.! class_ c

-- | 'H.tr' with class name.
tr_ :: H.AttributeValue -> O.Map H.Html
tr_ c = H.tr H.! class_ c

-- | 'H.td' with class name.
td_ :: H.AttributeValue -> O.Map H.Html
td_ c = H.td H.! class_ c

-- | 'H.pre' with class name.
pre_ :: H.AttributeValue -> O.Map H.Html
pre_ c = H.pre H.! class_ c


{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Write
  ( -- * Class
    Shortener, nullShortener,
    Write (..),
    writeDoc, writeString, writeHtml,

    -- * Html
    H.ToMarkup, H.toMarkup, H.toHtml,
    H.div, div_, H.span, span_,
    renderHtmlIndented,
    renderHtmlCompact,

    -- * Derivative
    writeH, writeV,
    writeColon, writeBar, writeSep,
    writeSepsWith,
    writeTerm, writeTerms,
    -- $Derivative

    -- * Simple writer
    doc, doch, docv,
    docWrap, docWraps,
    doc02, docConcat,
    -- $Simple
  ) where

import qualified Data.List                        as L
import qualified Text.Blaze                       as H
import qualified Text.Blaze.Html.Renderer.Pretty  as HI
import qualified Text.Blaze.Html.Renderer.String  as HC
import qualified Text.Blaze.XHtml5                as H
import qualified Text.Blaze.XHtml5.Attributes     as H (class_)
import qualified Text.PrettyPrint                 as D
import qualified Koshucode.Baala.Base.Prelude     as B


-- ----------------------  Data type

-- | Convert string to short sign.
type Shortener = String -> Maybe String

-- | Shortener which does not shorten strings.
nullShortener :: Shortener
nullShortener _ = Nothing

-- | Writer with shortener.
class Write a where
    writeDocWith :: Shortener -> a -> B.Doc

    writeStringWith :: Shortener -> a -> String
    writeStringWith sh a = show $ writeDocWith sh a

    writeHtmlWith :: Shortener -> a -> H.Html
    writeHtmlWith sh a = H.toHtml $ writeStringWith sh a

writeDoc :: (Write a) => a -> B.Doc
writeDoc = writeDocWith nullShortener

writeString :: (Write a) => a -> String
writeString = writeStringWith nullShortener

writeHtml :: (Write a) => a -> H.Html
writeHtml = writeHtmlWith nullShortener

instance Write B.Doc where
    writeDocWith _ x = x

instance Write Int where
    writeDocWith _ = D.int

instance Write Integer where
    writeDocWith _ = D.integer

instance Write String where
    writeDocWith _ = D.text

instance Write Bool where
    writeDocWith _ True  = D.text "(+)"
    writeDocWith _ False = D.text "(-)"

instance (Write a) => Write (B.Named a) where
    writeDocWith sh = writeTerm $ writeDocWith sh


-- ----------------------  Html

div_ :: H.AttributeValue -> B.Map H.Html
div_ c = H.div H.! H.class_ c

span_ :: H.AttributeValue -> B.Map H.Html
span_ c = H.span H.! H.class_ c

renderHtmlIndented :: H.Html -> String
renderHtmlIndented = HI.renderHtml

renderHtmlCompact :: H.Html -> String
renderHtmlCompact  = HC.renderHtml


-- ----------------------  Derivative

-- $Derivative
--
--   /Examples/
--
--   Colon-seperated document.
--
--     >>> writeColon id [True, False]
--     <1> : <0>
--
--   List of terms.
--
--     >>> writeTerms doc [("x", False), ("y", True)]
--     /x <0> /y <1>

writeH :: (Write a) => Shortener -> [a] -> B.Doc
writeH sh = D.hsep . map (writeDocWith sh)

writeV :: (Write a) => Shortener -> [a] -> B.Doc
writeV sh = D.vcat . map (writeDocWith sh)

writeColon :: (Write a) => Shortener -> [a] -> B.Doc
writeColon sh = writeSep ":" sh

writeBar :: (Write a) => Shortener -> [a] -> B.Doc
writeBar sh = writeSep "|" sh

writeSep :: (Write a) => String -> Shortener -> [a] -> B.Doc
writeSep sep sh = D.hsep . writeSeps sep sh

writeSeps :: (Write a) => String -> Shortener -> [a] -> [B.Doc]
writeSeps sep sh = writeSepsWith (writeDocWith sh) sep

writeSepsWith :: (Write a) => (a -> B.Doc) -> String -> [a] -> [B.Doc]
writeSepsWith w sep = L.intersperse (D.text sep) . map w

writeTerm :: (a -> B.Doc) -> B.Named a -> B.Doc
writeTerm w (n, x) = D.text ('/' : n) B.<+> w x

writeTerms :: (a -> B.Doc) -> [B.Named a] -> B.Doc
writeTerms w = doch . (writeTerm w `map`)



-- ----------------------  Simple writer

-- $Simple
--
--  Simple writers are writers with identity string mapping.
--  For example, @doc@ is equivalent for @write id@.
--
--  /Examples/
--
--  Wrap in open and close brackets.
--
--     >>> docWrap "[" "]" "abc"
--     [abc]
--
--  Put spaces between content and brackets.
--
--     >>> docWraps "[" "]" "abc"
--     [ abc ]

doc :: (Write a) => a -> B.Doc
doc = writeDocWith nullShortener

docv :: (Write a) => [a] -> B.Doc
docv = writeV nullShortener

doch :: (Write a) => [a] -> B.Doc
doch = writeH nullShortener

docWrap :: (Write a) => String -> String -> a -> B.Doc
docWrap = docWrapBody (B.<>)

docWraps :: (Write a) => String -> String -> a -> B.Doc
docWraps = docWrapBody (B.<+>)

docWrapBody :: (Write a) => (B.Doc -> B.Doc -> B.Doc) -> String -> String -> a -> B.Doc
docWrapBody p open close a = D.text open `p` doc a `p` D.text close

doc02 :: Int -> B.Doc
doc02 n | n < 10    = doc $ '0' : show n
        | otherwise = doc n

docConcat :: String -> B.Bin B.Doc
docConcat sep x y = x B.<> doc sep B.<> y


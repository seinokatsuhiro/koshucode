{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Html
  ( -- * Html
    H.ToMarkup, H.toMarkup,
    H.div, div_,
    H.span, span_,
  ) where

import qualified Text.Blaze                       as H
import qualified Text.Blaze.XHtml5                as H
import qualified Text.Blaze.XHtml5.Attributes     as H (class_)
import qualified Koshucode.Baala.Base.Prelude     as B

-- | 'H.div' with class name.
div_ :: H.AttributeValue -> B.Map H.Html
div_ c = H.div H.! H.class_ c

-- | 'H.span' with class name.
span_ :: H.AttributeValue -> B.Map H.Html
span_ c = H.span H.! H.class_ c


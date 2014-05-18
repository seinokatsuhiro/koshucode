{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Pretty
( -- * Class
  doc,
  doch,
  docv,

  -- * Function
  docColon,
  docWrap,
  docWraps,

  -- * Excerpt from PrettyPrint
  docEmpty,
  docHang,
  docZero,
) where

import qualified Data.List as L
import qualified Text.PrettyPrint as D
import qualified Koshucode.Baala.Base.Text.ShortDoc as B

doc :: (B.ShortDoc a) => a -> D.Doc
doc = B.shortDoc []

docv :: (B.ShortDoc a) => [a] -> D.Doc
docv = B.shortDocV []

doch :: (B.ShortDoc a) => [a] -> D.Doc
doch = B.shortDocH []

docEmpty :: D.Doc
docEmpty = D.empty

docHang :: D.Doc -> Int -> D.Doc -> D.Doc
docHang = D.hang

docZero :: String -> D.Doc
docZero = D.zeroWidthText

-- | Colon-seperated document.
--
--   >>> docBracket $ docColon [True, False]
--   [ #true : #false ]
docColon :: (B.ShortDoc a) => [a] -> D.Doc
docColon = D.hsep . docColons

-- | Colon-seperated list.
docColons :: (B.ShortDoc a) => [a] -> [D.Doc]
docColons = L.intersperse (D.text ":") . map doc

-- | Wrap in open and close brackets.
--   Put spaces between content and brackets.
--
--   >>> docWraps "(" ")" "abc"
--   ( abc )
docWraps :: (B.ShortDoc a) => String -> String -> a -> D.Doc
docWraps open close a = D.text open D.<+> doc a D.<+> D.text close

-- | Wrap in open and close brackets.
--
--   >>> docWrap "(" ")" "abc"
--   (abc)
docWrap :: (B.ShortDoc a) => String -> String -> a -> D.Doc
docWrap open close a = D.text open D.<> doc a D.<> D.text close


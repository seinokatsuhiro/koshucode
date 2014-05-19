{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.ShortDoc
( ShortDoc (..),
  ShortDef,
  shortDocH, shortDocV,
  shortDocColon,

  doc, doch, docv,
  docColon, docWrap, docWraps,
  docEmpty,docHang, docZero,
) where

import qualified Data.List                         as L
import qualified Text.PrettyPrint                  as D
import qualified Koshucode.Baala.Base.Prelude      as B

type ShortDef = B.Named String

class ShortDoc a where
    shortDoc :: [ShortDef] -> a -> B.Doc

instance ShortDoc B.Doc where
    shortDoc _ x = x

instance ShortDoc Int where
    shortDoc _ = D.int

instance ShortDoc String where
    shortDoc _ = D.text

instance ShortDoc Bool where
    shortDoc _ True  = D.text "#true"
    shortDoc _ False = D.text "#false"

instance (ShortDoc a) => ShortDoc (B.Named a) where
    shortDoc sh (n, x) = D.text n D.<+> shortDoc sh x

shortDocColon :: (ShortDoc a) => [ShortDef] -> [a] -> D.Doc
shortDocColon sh = D.hsep . shortDocColons sh

shortDocColons :: (ShortDoc a) => [ShortDef] -> [a] -> [D.Doc]
shortDocColons sh = L.intersperse (D.text ":") . map (shortDoc sh)

shortDocH :: (ShortDoc a) => [ShortDef] -> [a] -> D.Doc
shortDocH sh = D.hsep . map (shortDoc sh)

shortDocV :: (ShortDoc a) => [ShortDef] -> [a] -> D.Doc
shortDocV sh = D.vcat . map (shortDoc sh)

doc :: (ShortDoc a) => a -> D.Doc
doc = shortDoc []

docv :: (ShortDoc a) => [a] -> D.Doc
docv = shortDocV []

doch :: (ShortDoc a) => [a] -> D.Doc
doch = shortDocH []

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
docColon :: (ShortDoc a) => [a] -> D.Doc
docColon = D.hsep . docColons

-- | Colon-seperated list.
docColons :: (ShortDoc a) => [a] -> [D.Doc]
docColons = L.intersperse (D.text ":") . map doc

-- | Wrap in open and close brackets.
--   Put spaces between content and brackets.
--
--   >>> docWraps "(" ")" "abc"
--   ( abc )
docWraps :: (ShortDoc a) => String -> String -> a -> D.Doc
docWraps open close a = D.text open D.<+> doc a D.<+> D.text close

-- | Wrap in open and close brackets.
--
--   >>> docWrap "(" ")" "abc"
--   (abc)
docWrap :: (ShortDoc a) => String -> String -> a -> D.Doc
docWrap open close a = D.text open D.<> doc a D.<> D.text close


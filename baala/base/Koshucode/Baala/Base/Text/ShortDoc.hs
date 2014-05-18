{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.ShortDoc
( ShortDoc (..),
  ShortDef,
  shortText,
  shortDocH,
  shortDocV,
  shortDocColon,
) where

import qualified Data.List                         as L
import qualified Text.PrettyPrint                  as D
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text.Pretty  as B

type ShortDef = B.Named String

class (B.Pretty a) => ShortDoc a where
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

shortText :: [ShortDef] -> B.Map String
shortText = loop where
    loop [] s = '\'' : s
    loop ((prefix, long) : sh) s =
        case L.stripPrefix long s of
          Just s2 | s2 /= "" -> prefix ++ "." ++ s2
          _ -> loop sh s

shortDocColon :: (ShortDoc a) => [ShortDef] -> [a] -> D.Doc
shortDocColon sh = D.hsep . shortDocColons sh

shortDocColons :: (ShortDoc a) => [ShortDef] -> [a] -> [D.Doc]
shortDocColons sh = L.intersperse (D.text ":") . map (shortDoc sh)

shortDocH :: (ShortDoc a) => [ShortDef] -> [a] -> D.Doc
shortDocH sh = D.hsep . map (shortDoc sh)

shortDocV :: (ShortDoc a) => [ShortDef] -> [a] -> D.Doc
shortDocV sh = D.vcat . map (shortDoc sh)


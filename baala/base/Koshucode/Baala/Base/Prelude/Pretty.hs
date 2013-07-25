{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Prelude.Pretty
( Pretty (..),
  docTag,
  docParen, docBracket, docBrace,
  docAngle, docAngleBar,
  docColonList,
  docQuote,
  docComment,
  module Text.PrettyPrint,
) where

import qualified Data.List as L
import Text.PrettyPrint

class Pretty a where
    doc :: a -> Doc
    docv, doch :: [a] -> Doc
    docv = vcat . map doc
    doch = hsep . map doc

instance Pretty Doc where
    doc = id

docTag :: String -> Doc -> Doc
docTag tag doc2 = docParen $ text tag <+> doc2

docEnclose :: String -> String -> Doc -> Doc
docEnclose open close doc2 = text open <> doc2 <> text close

docColonList :: (Pretty a) => [a] -> [Doc]
docColonList = L.intersperse (text ":") . map doc

docParen, docBracket, docBrace :: Doc -> Doc
docAngle, docAngleBar          :: Doc -> Doc
docParen     =  docEnclose "(" ")"
docBracket   =  docEnclose "[" "]"
docBrace     =  docEnclose "{" "}"
docAngle     =  docEnclose "<" ">"
docAngleBar  =  docEnclose "<|" "|>"

docQuote :: Doc -> Doc
docQuote = docEnclose "'" "'"

docComment :: Doc -> Doc
docComment = docEnclose "%{" "}%"


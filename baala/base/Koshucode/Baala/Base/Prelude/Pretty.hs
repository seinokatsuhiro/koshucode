{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Prelude.Pretty
( -- * Class
  Pretty (..),

  -- * Function
  docTag,
  docColonList,
  docParen, docBracket, docBrace,
  docAngle, docAngleBar,
  docQuote,
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

docColonList :: (Pretty a) => [a] -> [Doc]
docColonList = L.intersperse (text ":") . map doc



-- ----------------------  Enclose

docEnclose :: String -> String -> Doc -> Doc
docEnclose open close doc2 = text open <+> doc2 <+> text close

{-| Enclose document in @(@ and @)@. -}
docParen     :: Doc -> Doc
docParen     =  docEnclose "(" ")"

{-| Enclose document in @[@ and @]@. -}
docBracket   :: Doc -> Doc
docBracket   =  docEnclose "[" "]"

{-| Enclose document in @{@ and @}@. -}
docBrace     :: Doc -> Doc
docBrace     =  docEnclose "{" "}"

{-| Enclose document in @\<@ and @\>@. -}
docAngle     :: Doc -> Doc
docAngle     =  docEnclose "<" ">"

{-| Enclose document in @\<|@ and @|\>@. -}
docAngleBar  :: Doc -> Doc
docAngleBar  =  docEnclose "<|" "|>"

{-| Enclose document in @\"@ and @\"@. -}
docQuote     :: Doc -> Doc
docQuote     = docEnclose "\"" "\""


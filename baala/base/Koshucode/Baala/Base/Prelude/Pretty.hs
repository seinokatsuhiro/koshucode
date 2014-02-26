{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Prelude.Pretty
( -- * Class
  Pretty (..),

  -- * Function
  docColon,
  docWrap,
  docWraps,

  -- * Excerpt from PrettyPrint
  D.Doc,
  (D.<>), (D.<+>), (D.$$), D.nest,
  docEmpty,
  docHang,
  docZero,
) where

import qualified Data.List as L
import qualified Text.PrettyPrint as D



-- ----------------------  Class

{-| Type that has a pretty printer. -}
class Pretty a where
    {-| 'D.Doc' of @a@. -}
    doc :: a -> D.Doc

    {-| 'D.Doc' joined by 'vcat'. -}
    docv :: [a] -> D.Doc
    docv = D.vcat . map doc

    {-| 'D.Doc' joined by 'hsep'. -}
    doch :: [a] -> D.Doc
    doch = D.hsep . map doc

{-| 'D.Doc' itself. -}
instance Pretty D.Doc where
    doc = id

{-| Same as 'D.int'.  -}
instance Pretty Int where
    doc = D.int

{-| Same as 'D.text'.  -}
instance Pretty String where
    doc = D.text

{-| >>> doch [True, False]
    #true #false  -}
instance Pretty Bool where
    doc True  = D.text "#true"
    doc False = D.text "#false"

{-| >>> doc ("/a", "xxx")
    /a xxx  -}
instance (Pretty a) => Pretty (String, a) where
    doc (n, x) = D.text n D.<+> doc x



-- ----------------------  Function

docEmpty :: D.Doc
docEmpty = D.empty

docHang :: D.Doc -> Int -> D.Doc -> D.Doc
docHang = D.hang

docZero :: String -> D.Doc
docZero = D.zeroWidthText

{-| Colon-seperated document.

    >>> docBracket $ docColon [True, False]
    [ #true : #false ]  -}
docColon :: (Pretty a) => [a] -> D.Doc
docColon = D.hsep . docColons

{-| Colon-seperated list. -}
docColons :: (Pretty a) => [a] -> [D.Doc]
docColons = L.intersperse (D.text ":") . map doc

{-| Wrap in open and close brackets.
    Put spaces between content and brackets.

    >>> docWraps "(" ")" "abc"
    ( abc )  -}
docWraps :: (Pretty a) => String -> String -> a -> D.Doc
docWraps open close a = D.text open D.<+> doc a D.<+> D.text close

{-| Wrap in open and close brackets.

    >>> docWrap "(" ")" "abc"
    (abc)  -}
docWrap :: (Pretty a) => String -> String -> a -> D.Doc
docWrap open close a = D.text open D.<> doc a D.<> D.text close


{-# OPTIONS_GHC -Wall #-}

{-| Source information -}

module Koshucode.Baala.Base.Abort.Source
( AbortSymbol (..)
) where

import Koshucode.Baala.Base.Prelude as Doc

{-| Class that represents abort reason. -}
class AbortSymbol a where
    abortSymbol  :: a -> String
    abortTitle   :: a -> String
    abortMain    :: a -> Doc
    abortSub     :: a -> Doc
    abortSub _   = Doc.empty


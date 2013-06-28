{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

{-| Source information -}

module Koshucode.Baala.Base.Abort.Source
( SourceLine (..)
, AbortSymbol (..)
) where
import Data.Generics
import Koshucode.Baala.Base.Prelude.Pretty as Doc

{-| Line number and content in source code -}
data SourceLine
    = SourceLine Int String
      deriving (Show, Eq, Ord, Data, Typeable)

instance Pretty SourceLine where
    doc (SourceLine _ line) = text line

{-| Class that represents abort reason. -}
class AbortSymbol a where
    abortSymbol  :: a -> String
    abortTitle   :: a -> String
    abortMain    :: a -> Doc
    abortSub     :: a -> Doc
    abortSub _ = Doc.empty
    abortLines   :: a -> [SourceLine]


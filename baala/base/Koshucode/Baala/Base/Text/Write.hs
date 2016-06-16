{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Write
  ( Write (..),
    doc, doch, docv,
    docWraps,
  ) where

import qualified Text.PrettyPrint                 as D
import qualified Koshucode.Baala.Base.Prelude     as B

-- | Writer with shortener.
class Write a where
    writeDocWith :: a -> B.Doc

instance Write B.Doc where
    writeDocWith x = x

instance Write Int where
    writeDocWith = D.int

instance Write Integer where
    writeDocWith = D.integer

instance Write String where
    writeDocWith = D.text

doc :: (Write a) => a -> B.Doc
doc = writeDocWith

docv :: (Write a) => [a] -> B.Doc
docv = D.vcat . map writeDocWith

doch :: (Write a) => [a] -> B.Doc
doch = D.hsep . map writeDocWith

docWraps :: (Write a) => String -> String -> a -> B.Doc
docWraps = docWrapBody (B.<+>)

docWrapBody :: (Write a) => (B.Doc -> B.Doc -> B.Doc) -> String -> String -> a -> B.Doc
docWrapBody p open close a = D.text open `p` doc a `p` D.text close


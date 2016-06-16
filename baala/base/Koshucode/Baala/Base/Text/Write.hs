{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Write
  ( -- * Class
    Shorten, noShorten,
    Write (..),

    -- * Simple writer
    doc, doch, docv,
    docWraps,
  ) where

import qualified Text.PrettyPrint                 as D
import qualified Koshucode.Baala.Base.Prelude     as B


-- ----------------------  Data type

-- | Convert string to short sign.
type Shorten = String -> Maybe String

-- | Shorten which does not shorten strings.
noShorten :: Shorten
noShorten _ = Nothing

-- | Writer with shortener.
class Write a where
    writeDocWith :: Shorten -> a -> B.Doc

instance Write B.Doc where
    writeDocWith _ x = x

instance Write Int where
    writeDocWith _ = D.int

instance Write Integer where
    writeDocWith _ = D.integer

instance Write String where
    writeDocWith _ = D.text


-- ----------------------  Derivative

writeH :: (Write a) => Shorten -> [a] -> B.Doc
writeH sh = D.hsep . map (writeDocWith sh)

writeV :: (Write a) => Shorten -> [a] -> B.Doc
writeV sh = D.vcat . map (writeDocWith sh)


-- ----------------------  Simple writer

doc :: (Write a) => a -> B.Doc
doc = writeDocWith noShorten

docv :: (Write a) => [a] -> B.Doc
docv = writeV noShorten

doch :: (Write a) => [a] -> B.Doc
doch = writeH noShorten

docWraps :: (Write a) => String -> String -> a -> B.Doc
docWraps = docWrapBody (B.<+>)

docWrapBody :: (Write a) => (B.Doc -> B.Doc -> B.Doc) -> String -> String -> a -> B.Doc
docWrapBody p open close a = D.text open `p` doc a `p` D.text close


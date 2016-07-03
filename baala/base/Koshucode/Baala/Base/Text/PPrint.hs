{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Convert to 'B.Doc' pretty printer.

module Koshucode.Baala.Base.Text.PPrint
  ( PPrint (..),
    doch, docv,
    docWraps,
  ) where

import qualified Text.PrettyPrint                 as Pr
import qualified Koshucode.Baala.Base.Prelude     as B
import qualified Koshucode.Baala.Base.List        as B

-- | Convert to 'B.Doc' pretty printer.
class PPrint a where
    doc :: a -> B.Doc

-- | No conversion.
instance PPrint B.Doc where
    doc = id

-- | 'Int' printer, same as 'Pr.int'.
instance PPrint Int where
    doc = Pr.int

-- | 'Integer' printer, same as 'Pr.integer'.
instance PPrint Integer where
    doc = Pr.integer

-- | 'String' printer, same as 'Pr.text'.
instance PPrint String where
    doc = Pr.text

-- | Concatenate docs horizontally with space, same as 'Pr.hsep'.
--
--   >>> doch ["abc", "def"]
--   abc def

doch :: (PPrint a) => [a] -> B.Doc
doch = Pr.hsep . map doc

-- | Concatenate docs vertically, same as 'Pr.vcat'.
--
--   >>> docv ["abc", "def"]
--   abc
--   def

docv :: (PPrint a) => [a] -> B.Doc
docv = Pr.vcat . map doc

-- | Wrap doc with open and close docs.
--
--   >>> docWraps "(" ")" "abc"
--   ( abc )

docWraps :: (PPrint a, PPrint b) => a -> a -> b -> B.Doc
docWraps = docWrapBody (B.<+>)

docWrapBody :: (PPrint a, PPrint b) => B.Bin B.Doc -> a -> a -> b -> B.Doc
docWrapBody p open close a = doc open `p` doc a `p` doc close


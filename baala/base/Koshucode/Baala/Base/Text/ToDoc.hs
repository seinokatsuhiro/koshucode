{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Convert to 'B.Doc' pretty printer.

module Koshucode.Baala.Base.Text.ToDoc
  ( ToDoc (..),
    doch, docv,
    docWraps,
  ) where

import qualified Text.PrettyPrint                 as Pr
import qualified Koshucode.Baala.Base.Prelude     as B
import qualified Koshucode.Baala.Base.List        as B

-- | Convert to 'B.Doc' pretty printer.
class ToDoc a where
    doc :: a -> B.Doc

-- | No conversion.
instance ToDoc B.Doc where
    doc = id

-- | 'Int' printer, same as 'Pr.int'.
instance ToDoc Int where
    doc = Pr.int

-- | 'Integer' printer, same as 'Pr.integer'.
instance ToDoc Integer where
    doc = Pr.integer

-- | 'String' printer, same as 'Pr.text'.
instance ToDoc String where
    doc = Pr.text

-- | Concatenate docs horizontally with space, same as 'Pr.hsep'.
--
--   >>> doch ["abc", "def"]
--   abc def

doch :: (ToDoc a) => [a] -> B.Doc
doch = Pr.hsep . map doc

-- | Concatenate docs vertically, same as 'Pr.vcat'.
--
--   >>> docv ["abc", "def"]
--   abc
--   def

docv :: (ToDoc a) => [a] -> B.Doc
docv = Pr.vcat . map doc

-- | Wrap doc with open and close docs.
--
--   >>> docWraps "(" ")" "abc"
--   ( abc )

docWraps :: (ToDoc a, ToDoc b) => a -> a -> b -> B.Doc
docWraps = docWrapBody (B.<+>)

docWrapBody :: (ToDoc a, ToDoc b) => B.Bin B.Doc -> a -> a -> b -> B.Doc
docWrapBody p open close a = doc open `p` doc a `p` doc close


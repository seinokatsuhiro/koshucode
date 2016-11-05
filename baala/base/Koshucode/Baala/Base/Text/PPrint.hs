{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Convert to 'B.Doc' pretty printer.

module Koshucode.Baala.Base.Text.PPrint
  ( PPrint (..),
    pprintH, pprintV,
    pprintWraps,
  ) where

import qualified Text.PrettyPrint                 as Pr
import qualified Koshucode.Baala.Overture         as O
import qualified Koshucode.Baala.Base.Prelude     as B

-- | Convert to 'B.Doc' pretty printer.
class PPrint a where
    pprint :: a -> B.Doc

-- | No conversion.
instance PPrint B.Doc where
    pprint = id

-- | 'Int' printer, same as 'Pr.int'.
instance PPrint Int where
    pprint = Pr.int

-- | 'Integer' printer, same as 'Pr.integer'.
instance PPrint Integer where
    pprint = Pr.integer

-- | 'String' printer, same as 'Pr.text'.
instance PPrint String where
    pprint = Pr.text

-- | Concatenate docs horizontally with space, same as 'Pr.hsep'.
--
--   >>> pprintH ["abc", "def"]
--   abc def

pprintH :: (PPrint a) => [a] -> B.Doc
pprintH = Pr.hsep . map pprint

-- | Concatenate docs vertically, same as 'Pr.vcat'.
--
--   >>> pprintV ["abc", "def"]
--   abc
--   def

pprintV :: (PPrint a) => [a] -> B.Doc
pprintV = Pr.vcat . map pprint

-- | Wrap doc with open and close docs.
--
--   >>> pprintWraps "(" ")" "abc"
--   ( abc )

pprintWraps :: (PPrint a, PPrint b) => a -> a -> b -> B.Doc
pprintWraps = pprintWrapBody (B.<+>)

pprintWrapBody :: (PPrint a, PPrint b) => O.Bin B.Doc -> a -> a -> b -> B.Doc
pprintWrapBody p open close a = pprint open `p` pprint a `p` pprint close


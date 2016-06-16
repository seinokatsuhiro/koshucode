{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Convert to 'B.Doc'.

module Koshucode.Baala.Base.Text.ToDoc
  ( ToDoc (..),
    doc, doch, docv,
    docWraps,
  ) where

import qualified Text.PrettyPrint                 as Pr
import qualified Koshucode.Baala.Base.Prelude     as B
import qualified Koshucode.Baala.Base.List        as B

-- | Convert to pretty print 'B.Doc'.
class ToDoc a where
    toDoc :: a -> B.Doc

-- | Identity.
instance ToDoc B.Doc where
    toDoc = id

-- | Same as 'Pr.int'.
instance ToDoc Int where
    toDoc = Pr.int

-- | Same as 'Pr.intteger'.
instance ToDoc Integer where
    toDoc = Pr.integer

-- | Same as 'Pr.text'.
instance ToDoc String where
    toDoc = Pr.text

doc :: (ToDoc a) => a -> B.Doc
doc = toDoc

docv :: (ToDoc a) => [a] -> B.Doc
docv = Pr.vcat . map toDoc

doch :: (ToDoc a) => [a] -> B.Doc
doch = Pr.hsep . map toDoc

docWraps :: (ToDoc a, ToDoc b) => a -> a -> b -> B.Doc
docWraps = docWrapBody (B.<+>)

docWrapBody :: (ToDoc a, ToDoc b) => B.Bin B.Doc -> a -> a -> b -> B.Doc
docWrapBody p open close a = toDoc open `p` toDoc a `p` toDoc close


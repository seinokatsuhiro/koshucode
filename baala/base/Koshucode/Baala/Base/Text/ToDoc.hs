{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Convert to 'B.Doc'.

module Koshucode.Baala.Base.Text.ToDoc
  ( ToDoc (..),
    doch, docv,
    docWraps,
  ) where

import qualified Text.PrettyPrint                 as Pr
import qualified Koshucode.Baala.Base.Prelude     as B
import qualified Koshucode.Baala.Base.List        as B

-- | Convert to pretty print 'B.Doc'.
class ToDoc a where
    doc :: a -> B.Doc

-- | Identity.
instance ToDoc B.Doc where
    doc = id

-- | Same as 'Pr.int'.
instance ToDoc Int where
    doc = Pr.int

-- | Same as 'Pr.intteger'.
instance ToDoc Integer where
    doc = Pr.integer

-- | Same as 'Pr.text'.
instance ToDoc String where
    doc = Pr.text

docv :: (ToDoc a) => [a] -> B.Doc
docv = Pr.vcat . map doc

doch :: (ToDoc a) => [a] -> B.Doc
doch = Pr.hsep . map doc

docWraps :: (ToDoc a, ToDoc b) => a -> a -> b -> B.Doc
docWraps = docWrapBody (B.<+>)

docWrapBody :: (ToDoc a, ToDoc b) => B.Bin B.Doc -> a -> a -> b -> B.Doc
docWrapBody p open close a = doc open `p` doc a `p` doc close


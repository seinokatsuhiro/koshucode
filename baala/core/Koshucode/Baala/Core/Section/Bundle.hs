{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Section.Bundle
  ( SectionBundle (..),
    readSectionBundle,
    bundleFiles,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Section.Section as C
import qualified Koshucode.Baala.Core.Section.Read    as C

-- | Bundle of section resources.
data SectionBundle c = SectionBundle
    { bundleRoot      :: C.Section c
    , bundleResources :: [B.Resource]
    } deriving (Show)

readSectionBundle :: (C.CContent c) => SectionBundle c -> IO [B.Ab (C.Section c)]
readSectionBundle bun =
    do let root = bundleRoot bun
       C.readSection root `mapM` bundleResources bun

bundleFiles :: SectionBundle c -> [String]
bundleFiles = B.mapMaybe path . map B.resourceName . bundleResources where
    path (B.ResourceFile p) = Just p
    path _                  = Nothing

{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Section.Bundle
( SectionBundle (..),
  readSectionBundle,
) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Section.Section as C
import qualified Koshucode.Baala.Core.Section.Read    as C

-- | Bundle of section resources.
data SectionBundle c = SectionBundle
    { bundleRoot  :: C.Section c
    , bundleTexts :: [String]
    , bundleFiles :: [FilePath]
    , bundleURLs  :: [String]
    } deriving (Show)

readSectionBundle :: (C.CContent c) => SectionBundle c -> IO [B.Ab (C.Section c)]
readSectionBundle src =
    do let root = bundleRoot src
       mapM (C.readSection root) $ bundleResources src

bundleResources :: SectionBundle c -> [B.Resource]
bundleResources src = texts ++ files ++ urls where
    texts = (B.Resource 0 . B.ResourceText) `map` bundleTexts src
    files = (B.Resource 0 . B.ResourceFile) `map` bundleFiles src
    urls  = (B.Resource 0 . B.ResourceURL)  `map` bundleURLs  src


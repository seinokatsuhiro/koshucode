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
readSectionBundle bun =
    do let root  = bundleRoot  bun
           names = bundleNames bun
       C.readSection root `mapM` resources names

resources :: [B.ResourceName] -> [B.Resource]
resources = zipWith B.Resource [1..]

bundleNames :: SectionBundle c -> [B.ResourceName]
bundleNames bun = texts ++ files ++ urls where
    texts  =  B.ResourceText `map` bundleTexts bun
    files  =  B.ResourceFile `map` bundleFiles bun
    urls   =  B.ResourceURL  `map` bundleURLs  bun

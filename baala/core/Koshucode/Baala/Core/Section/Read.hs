{-# OPTIONS_GHC -Wall #-}

-- | Read source as section.

module Koshucode.Baala.Core.Section.Read
  ( -- * Section
    readSection,
    readSectionText,

    -- * Bundle
    SourceBundle (..),
    bundleTexts,
    bundleRead,
  ) where

import qualified System.Directory                     as Dir
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Section.Section as C
import qualified Koshucode.Baala.Core.Section.Clause  as C
import qualified Koshucode.Baala.Core.Message         as Msg


-- ----------------------  Section

-- | Read section from certain source.
readSection :: (C.CContent c) => C.Section c -> B.Source -> IO (B.Ab (C.Section c))
readSection root res = dispatch $ B.sourceName res where
    dispatch (B.SourceFile path)
        = do exist <- Dir.doesFileExist path
             case exist of
               False  ->  return $ Msg.noFile path
               True   ->  ioRead =<< readFile path

    dispatch (B.SourceText code)  =  ioRead code
    dispatch (B.SourceStdin)      =  ioRead =<< getContents
    dispatch (B.SourceURL _)      =  error "Not implemented read from URL"

    ioRead = return . readSectionCode root res

readSectionCode :: (C.CContent c)
    => C.Section c -> B.Source -> String -> B.Ab (C.Section c)
readSectionCode root res =
    C.consSection root res . C.consClause B.<=< B.tokenLines res

-- | Read section from text.
readSectionText :: (C.CContent c) => C.Section c -> String -> B.Ab (C.Section c)
readSectionText root code = readSectionCode root (B.sourceOf code) code


-- ----------------------  Bundle

-- | Bundle of sources.
data SourceBundle c = SourceBundle
    { bundleRoot      :: C.Section c
    , bundleSources :: [B.Source]
    } deriving (Show)

bundleTexts :: SourceBundle c -> [String]
bundleTexts = map B.sourceText . bundleSources

bundleRead :: (C.CContent c) => SourceBundle c -> IO (B.Ab (C.Section c))
bundleRead bun =
    do let root = bundleRoot bun
       abSects <- readSection root `mapM` bundleSources bun
       return $ do sects <- B.sequence abSects
                   return $ C.concatSection root sects


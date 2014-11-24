{-# OPTIONS_GHC -Wall #-}

-- | Read source as resource.

module Koshucode.Baala.Core.Section.Read
  ( -- * Resource
    readResource,
    readResourceText,

    -- * Bundle
    SourceBundle (..),
    bundleTexts,
    bundleRead,
  ) where

import qualified System.Directory                      as Dir
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Content          as C
import qualified Koshucode.Baala.Core.Section.Resource as C
import qualified Koshucode.Baala.Core.Section.Clause   as C
import qualified Koshucode.Baala.Core.Message          as Msg


-- ----------------------  Resource

-- | Read resource from certain source.
readResource :: (C.CContent c) => C.Resource c -> B.Source -> IO (B.Ab (C.Resource c))
readResource root res = dispatch $ B.sourceName res where
    dispatch (B.SourceFile path)
        = do exist <- Dir.doesFileExist path
             case exist of
               False  ->  return $ Msg.noFile path
               True   ->  ioRead =<< readFile path

    dispatch (B.SourceText code)  =  ioRead code
    dispatch (B.SourceStdin)      =  ioRead =<< getContents
    dispatch (B.SourceURL _)      =  error "Not implemented read from URL"

    ioRead = return . readResourceCode root res

readResourceCode :: (C.CContent c)
    => C.Resource c -> B.Source -> String -> B.Ab (C.Resource c)
readResourceCode root res =
    C.consResource root res . C.consClause B.<=< B.tokenLines res

-- | Read resource from text.
readResourceText :: (C.CContent c) => C.Resource c -> String -> B.Ab (C.Resource c)
readResourceText root code = readResourceCode root (B.sourceOf code) code


-- ----------------------  Bundle

-- | Bundle of sources.
data SourceBundle c = SourceBundle
    { bundleRoot      :: C.Resource c
    , bundleSources :: [B.Source]
    } deriving (Show)

bundleTexts :: SourceBundle c -> [String]
bundleTexts = map B.sourceText . bundleSources

bundleRead :: (C.CContent c) => SourceBundle c -> IO (B.Ab (C.Resource c))
bundleRead bun =
    do let root = bundleRoot bun
       abSects <- readResource root `mapM` bundleSources bun
       return $ do sects <- B.sequence abSects
                   return $ C.concatResource root sects


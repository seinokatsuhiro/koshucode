{-# OPTIONS_GHC -Wall #-}

-- | Read source as resource.

module Koshucode.Baala.Core.Resource.Read
  ( -- * Resource
    readResource,
    readResourceText,

    -- * Bundle
    SourceBundle (..),
    bundleTexts,
    bundleRead,
  ) where

import qualified System.Directory                       as Dir
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Content           as C
import qualified Koshucode.Baala.Core.Resource.Resource as C
import qualified Koshucode.Baala.Core.Resource.Clause   as C
import qualified Koshucode.Baala.Core.Message           as Msg


-- ----------------------  Resource

-- | Read resource from certain source.
readResource :: (C.CContent c) => C.Resource c -> B.Source -> IO (B.Ab (C.Resource c))
readResource res src = dispatch $ B.sourceName src where
    dispatch (B.SourceFile path)
        = do exist <- Dir.doesFileExist path
             case exist of
               False  ->  return $ Msg.noFile path
               True   ->  ioRead =<< readFile path

    dispatch (B.SourceText code)  =  ioRead code
    dispatch (B.SourceStdin)      =  ioRead =<< getContents
    dispatch (B.SourceURL _)      =  error "Not implemented read from URL"

    ioRead = return . readResourceCode res src

-- | Read resource from text.
readResourceText :: (C.CContent c) => C.Resource c -> String -> B.Ab (C.Resource c)
readResourceText res code = readResourceCode res (B.sourceOf code) code

readResourceCode :: (C.CContent c)
    => C.Resource c -> B.Source -> String -> B.Ab (C.Resource c)
readResourceCode res src =
    C.resInclude res src . C.consClause sec B.<=< B.tokenLines src where
        sec = C.resLastSecNo res + 1


-- ----------------------  Bundle

-- | Bundle of sources.
data SourceBundle c = SourceBundle
    { bundleRoot     :: C.Resource c
    , bundleSources  :: [B.Source]
    } deriving (Show)

bundleTexts :: SourceBundle c -> [String]
bundleTexts = map B.sourceText . bundleSources

bundleRead :: (C.CContent c) => SourceBundle c -> IO (B.Ab (C.Resource c))
bundleRead SourceBundle { bundleRoot = root, bundleSources = ss } = result where
    result             = merge root $ reverse ss
    merge res []       = return $ Right res
    merge res (s:src)  = do ab <- readResource res s
                            case ab of
                              Right res' -> merge res' src
                              left       -> return left


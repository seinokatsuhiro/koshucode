{-# OPTIONS_GHC -Wall #-}

-- | Read source as resource.

module Koshucode.Baala.Core.Resource.Read
  ( -- * Resource
    readResourceText,

    -- * Bundle
    SourceBundle (..),
    bundleTexts,
    bundleRead,
  ) where

import qualified System.Directory                        as Dir
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Core.Content            as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Core.Resource.Include   as C
import qualified Koshucode.Baala.Core.Message            as Msg


-- ----------------------  Resource

readResource :: (C.CContent c) => C.Resource c -> IO (B.Ab (C.Resource c))
readResource res@C.Resource { C.resArticle = (todo, _) } =
    case todo of
      []      -> return $ Right res
      src : _ -> do abres' <- readResourceOne (pop res) src
                    case abres' of
                      Right res' -> readResource $ push src res'
                      left       -> return left
    where
      pop        = call $ B.mapFst tail
      push src   = call $ B.consSnd src
      call f r   = r { C.resArticle = f $ C.resArticle r }

-- | Read resource from certain source.
readResourceOne :: (C.CContent c) => C.Resource c -> B.Source -> IO (B.Ab (C.Resource c))
readResourceOne res src = dispatch $ B.sourceName src where
    dispatch (B.SourceFile path)
        = do exist <- Dir.doesFileExist path
             case exist of
               False  ->  return $ Msg.noFile path
               True   ->  ioRead =<< readFile path

    dispatch (B.SourceText code)  =  ioRead code
    dispatch (B.SourceStdin)      =  ioRead =<< getContents
    dispatch (B.SourceURL _)      =  error "Not implemented read from URL"

    ioRead = return . C.resInclude res src

-- | Read resource from text.
readResourceText :: (C.CContent c) => C.Resource c -> String -> B.Ab (C.Resource c)
readResourceText res code = C.resInclude res (B.sourceOf code) code


-- ----------------------  Bundle

-- | Bundle of sources.
data SourceBundle c = SourceBundle
    { bundleRoot     :: C.Resource c
    , bundleSources  :: [B.Source]
    } deriving (Show)

bundleTexts :: SourceBundle c -> [String]
bundleTexts = map B.sourceText . bundleSources

bundleRead :: (C.CContent c) => SourceBundle c -> IO (B.Ab (C.Resource c))
bundleRead SourceBundle { bundleRoot = res, bundleSources = src }
    = readResource $ res { C.resArticle = (reverse src, []) }


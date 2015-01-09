{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Read resource.

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

readResource :: (C.CContent c) => Int -> C.Resource c -> B.IOAb (C.Resource c)
readResource n res@C.Resource { C.resArticle = (todo, srclist, done) }
    = case (todo, srclist, done) of
        ([], [], _)            -> return $ Right res
        (_ , [], _)            -> readDitto res { C.resArticle = ([], todo', done) }
        (_ , src : _, _)
            | src `elem` done  -> readDitto $ pop res
            | otherwise        -> do let src' = src { B.sourceNumber = n }
                                     abres' <- readResourceOne (pop res) src'
                                     case abres' of
                                       Right res' -> readUp $ push src' res'
                                       left       -> return left
      where
        readDitto               = readResource $ n
        readUp                  = readResource $ n + 1
        todo'                   = reverse todo
        pop                     = call $ map2 tail
        push                    = call . cons3
        call f r                = r { C.resArticle = f $ C.resArticle r }
        map2  f (a, b, c)       = (a, f b, c)
        cons3 x (a, b, c)       = (a, b, x : c)

-- | Read resource from certain source.
readResourceOne :: forall c. (C.CContent c) =>
    C.Resource c -> B.Source -> B.IOAb (C.Resource c)
readResourceOne res src = dispatch $ B.sourceName src where
    dispatch (B.SourceFile path) =
        do exist <- Dir.doesFileExist path
           case exist of
             True   -> include =<< readFile path
             False  -> return $ Msg.noFile path

    dispatch (B.SourceURL url) =
        do abcode <- B.uriContent url
           case abcode of
             Right code       -> include code
             Left (code, msg) -> return $ Msg.httpError url code msg

    dispatch (B.SourceText text)  = include text
    dispatch (B.SourceStdin)      = include =<< getContents

    include :: String -> B.IOAb (C.Resource c)
    include = return . C.resInclude res src

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

bundleRead :: (C.CContent c) => SourceBundle c -> B.IOAb (C.Resource c)
bundleRead SourceBundle { bundleRoot = res, bundleSources = src }
    = readResource 1 $ res { C.resArticle = ([], reverse src, []) }


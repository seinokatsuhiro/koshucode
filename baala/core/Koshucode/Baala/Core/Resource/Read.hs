{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Read resource.

module Koshucode.Baala.Core.Resource.Read
  ( -- * Resource
    ResourceIO, readResourceText,

    -- * Bundle
    SourceBundle (..),
    bundleTexts,
    bundleRead,
  ) where

import qualified System.Directory                        as Dir
import qualified Control.Monad.State                     as M
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Core.Content            as C
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Core.Resource.Include   as C
import qualified Koshucode.Baala.Core.Message            as Msg


-- ----------------------  Resource

type ResourceIO c = M.StateT (C.Global c) IO (B.Ab (C.Resource c))

readResource :: (C.CContent c) => Int -> C.Resource c -> ResourceIO c
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
    C.Resource c -> B.Source -> ResourceIO c
readResourceOne res src = dispatch $ B.sourceName src where
    dispatch (B.SourceFile path) =
        io $ do exist <- Dir.doesFileExist path
                case exist of
                  True   -> include =<< readFile path
                  False  -> return $ Msg.noFile path

    dispatch (B.SourceURL url) =
        do g <- M.get
           let proxy = C.globalProxy g
           abcode <- io $ B.uriContent proxy url
           io $ case abcode of
             Right code       -> include code
             Left (code, msg) -> return $ Msg.httpError url code msg

    dispatch (B.SourceText text)  = io $ include text
    dispatch (B.SourceStdin)      = io $ include =<< getContents

    include :: String -> B.IOAb (C.Resource c)
    include = return . C.resInclude res src

io :: IO a -> M.StateT (C.Global c) IO a
io = M.liftIO

-- | Read resource from text.
readResourceText :: (C.CContent c) => C.Resource c -> String -> B.Ab (C.Resource c)
readResourceText res code = C.resInclude res (B.sourceOf code) code


-- ----------------------  Bundle

-- | Bundle of sources.
data SourceBundle c = SourceBundle
    { bundleSources  :: [B.Source]
    } deriving (Show)

bundleTexts :: SourceBundle c -> [String]
bundleTexts = map B.sourceText . bundleSources

bundleRead :: forall c. (C.CContent c) => C.Global c -> SourceBundle c -> B.IOAb (C.Resource c)
bundleRead g SourceBundle { bundleSources = src } =
    do (res', _) <- M.runStateT proc g
       return res'
    where
      proc :: ResourceIO c
      proc  = readResource 1 $ res { C.resArticle = ([], reverse src, []) }
      res   = C.globalHook g


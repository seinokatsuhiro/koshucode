{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Read resource.

module Koshucode.Baala.Core.Resource.Read
  ( GlobalIO, ResourceIO,
    gio, gioResource,
    readResourceText,
    readSources,
  ) where

import qualified System.Directory                        as Dir
import qualified System.FilePath                         as Path
import qualified Control.Monad.State                     as M
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Core.Content            as C
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Core.Resource.Include   as C
import qualified Koshucode.Baala.Core.Message            as Msg


-- | I/O with global state.
type GlobalIO a c = M.StateT (C.Global c) IO a

-- | Calculation that returns abortable resource.
type ResourceIO c = GlobalIO (C.AbResource c) c

gio :: IO a -> GlobalIO a c
gio = M.liftIO

gioResource :: ResourceIO c -> C.Global c -> IO (C.AbResource c, C.Global c)
gioResource = M.runStateT

getRootResoruce :: GlobalIO (C.Resource c) c
getRootResoruce = return . C.globalHook =<< M.get

nextSourceCount :: GlobalIO Int c
nextSourceCount =
    do g <- M.get
       let n = 1 + C.globalSourceCount g
       M.put $ g { C.globalSourceCount = n }
       return n

readResource :: (C.CContent c) => C.Resource c -> ResourceIO c
readResource res@C.Resource { C.resInputStack = article@(todo, _, done) }
    = case article of
        ([], [], _)            -> return $ Right res
        (_ , [], _)            -> readResource res { C.resInputStack = ([], todo', done) }
        (_ , src : _, _)
            | B.CodePiece 0 src `elem` done
                               -> readResource $ pop res
            | otherwise        -> do n <- nextSourceCount
                                     let src' = B.CodePiece n src
                                     abres' <- readResourceOne (pop res) src'
                                     case abres' of
                                       Right res' -> readResource $ push src' res'
                                       left       -> return left
      where
        todo'                   = reverse todo
        pop                     = call $ map2 tail
        push                    = call . cons3
        call f r                = r { C.resInputStack = f $ C.resInputStack r }
        map2  f (a, b, c)       = (a, f b, c)
        cons3 x (a, b, c)       = (a, b, x : c)

-- | Read resource from certain source.
readResourceOne :: forall c. (C.CContent c) =>
    C.Resource c -> B.CodePiece -> ResourceIO c
readResourceOne res src = dispatch $ B.codeName src where
    dispatch (B.IOPointFile cd path) =
        gio $ do let path' = cd ++ path
                     cd'   = cd ++ Path.dropFileName path
                 exist <- Dir.doesFileExist path'
                 case exist of
                   True   -> includeUnder cd' =<< readFile path'
                   False  -> return $ Msg.noFile cd path

    dispatch (B.IOPointUri url) =
        do g <- M.get
           let proxy = C.globalProxy g
           abcode <- gio $ B.uriContent proxy url
           gio $ case abcode of
             Right code       -> include code
             Left (code, msg) -> return $ Msg.httpError url code msg

    dispatch (B.IOPointText text)  = gio $ include text
    dispatch (B.IOPointStdin)      = gio $ include =<< getContents
    dispatch (B.IOPointStdout)     = B.bug "readResourceOne"

    include :: FilePath -> IO (C.AbResource c)
    include = includeUnder ""

    includeUnder :: FilePath -> FilePath -> IO (C.AbResource c)
    includeUnder cd = return . C.resInclude cd res src

-- | Read resource from text.
readResourceText :: (C.CContent c) => C.Resource c -> String -> C.AbResource c
readResourceText res code = C.resInclude "" res (B.codeTextOf code) code

readSources :: forall c. (C.CContent c) => [B.IOPoint] -> ResourceIO c
readSources src =
    do res <- getRootResoruce
       readResource $ res { C.resInputStack = ([], reverse src, []) }


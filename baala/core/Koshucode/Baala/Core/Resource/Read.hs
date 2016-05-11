{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Read resource.

module Koshucode.Baala.Core.Resource.Read
  ( GlobalIO, ResourceIO,
    gio, gioResource,
    readResourceText,
    readSources,
  ) where

import qualified Control.Monad.State                     as M
import qualified Data.ByteString.Lazy                    as Bz
import qualified System.Directory                        as Dir
import qualified System.FilePath                         as Path
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Core.Resource.Include   as C
import qualified Koshucode.Baala.Core.Resource.Message   as Msg


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

readResource :: (D.CContent c) => C.Resource c -> ResourceIO c
readResource res@C.Resource { C.resInputStack = article@(todo, _, done) }
    = case article of
        ([], [], _)            -> return $ Right res
        (_ , [], _)            -> readResource res { C.resInputStack = ([], todo', done) }
        (_ , src : _, _)
            | B.CodePiece 0 srcPt `elem` done
                               -> readResource $ pop res
            | otherwise        -> do n <- nextSourceCount
                                     let src' = B.CodePiece n srcPt
                                     abres' <- readResourceOne (pop res) src' srcAbout
                                     case abres' of
                                       Right res' -> readResource $ push src' res'
                                       left       -> return left
            where srcPt         = C.inputPoint src
                  srcAbout      = C.inputPointAbout src
      where
        todo'                   = reverse todo
        pop                     = call $ map2 tail
        push                    = call . cons3
        call f r                = r { C.resInputStack = f $ C.resInputStack r }
        map2  f (a, b, c)       = (a, f b, c)
        cons3 x (a, b, c)       = (a, b, x : c)

-- | Read resource from certain source.
readResourceOne :: forall c. (D.CContent c) =>
    C.Resource c -> B.CodePiece -> [S.TTree] -> ResourceIO c
readResourceOne res src add = dispatch $ B.codeName src where
    dispatch (B.IOPointFile cd path) =
        gio $ do let path' = putDir cd path
                     cd'   = putDir cd $ Path.dropFileName path
                 exist <- Dir.doesFileExist path'
                 case exist of
                   True   -> includeUnder cd' =<< Bz.readFile path'
                   False  -> return $ Msg.noFile cd path

    dispatch (B.IOPointUri url) =
        do g <- M.get
           let proxy = C.globalProxy g
           abcode <- gio $ B.uriContent proxy url
           gio $ case abcode of
             Right code       -> include code
             Left (code, msg) -> return $ Msg.httpError url code msg

    dispatch (B.IOPointText   _ code) = gio $ include code
    dispatch (B.IOPointCustom _ code) = gio $ include code
    dispatch (B.IOPointStdin)         = gio $ include =<< Bz.getContents
    dispatch (B.IOPointStdout)        = B.bug "readResourceOne"

    putDir dir path  = cutDot dir ++ cutDot path

    cutDot ('.' : '/' : path) = cutDot path
    cutDot path               = path

    include :: B.Bz -> IO (C.AbResource c)
    include = includeUnder ""

    includeUnder :: FilePath -> B.Bz -> IO (C.AbResource c)
    includeUnder cd = return . C.resInclude add' cd res src

    add' = concatMap B.untree add

-- | Read resource from text.
readResourceText :: (D.CContent c) => C.Resource c -> String -> C.AbResource c
readResourceText res code = C.resInclude [] "" res (B.codeTextOf code') code' where
    code' = B.stringBz code

readSources :: forall c. (D.CContent c) => [B.IOPoint] -> ResourceIO c
readSources src =
    do res <- getRootResoruce
       readResource $ res { C.resInputStack = ([]
                            , map input $ reverse src
                            , []) }

input :: B.IOPoint -> C.InputPoint
input pt = C.InputPoint pt []


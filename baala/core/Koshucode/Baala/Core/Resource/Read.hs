{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Read resource.

module Koshucode.Baala.Core.Resource.Read
  ( GlobalIO, ResourceIO,
    gio, gioResource,
    readResource,
    readResourceText,
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

-- | Read resource from text.
readResourceText :: (D.CContent c) => C.Resource c -> String -> C.AbResource c
readResourceText res code = C.resInclude [] "" res (B.codeTextOf code') code' where
    code' = B.stringBz code

-- | Read relational resource.
readResource :: forall c. (D.CContent c) => [B.IOPoint] -> ResourceIO c
readResource src =
    do res <- getRootResoruce
       readStack $ res { C.resInputStack = ([], ready, []) }
    where
      ready = map input $ reverse src
      input pt = C.InputPoint pt []

-- | Read all on the input stack.
readStack :: (D.CContent c) => C.Resource c -> ResourceIO c
readStack res@C.Resource { C.resInputStack = stack } =
    case stack of
      ([], [], _)      -> return $ Right res
      (todo, [], done) -> readStack $ res { C.resInputStack = ([], reverse todo, done) }
      (todo, src : ready, done)
        | B.CodePiece 0 srcPt `elem` done
                         -> readStack pop  -- skip
        | otherwise      -> do n <- nextSourceCount
                               let src' = B.CodePiece n srcPt
                               abres' <- readCode pop src' srcAbout
                               case abres' of
                                 Right res' -> readStack $ C.resStackDone src' res'
                                 left       -> return left
        where srcPt      = C.inputPoint src
              srcAbout   = C.inputPointAbout src
              pop        = res { C.resInputStack = (todo, ready, done) }

-- | Read resource from certain source.
readCode :: forall c. (D.CContent c) =>
    C.Resource c -> B.CodePiece -> [S.TTree] -> ResourceIO c
readCode res src add = dispatch $ B.codeName src where
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
    dispatch (B.IOPointStdout)        = B.bug "read resource"

    putDir dir path  = cutDot dir ++ cutDot path

    cutDot ('.' : '/' : path) = cutDot path
    cutDot path               = path

    include :: B.Bz -> IO (C.AbResource c)
    include = includeUnder ""

    includeUnder :: FilePath -> B.Bz -> IO (C.AbResource c)
    includeUnder cd = return . C.resInclude add' cd res src

    add' = concatMap B.untree add


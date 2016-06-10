{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Read data resource.

module Koshucode.Baala.Core.Resource.Read
  ( -- * Read resource
    readResource,
    readResourceSingle,
    readResourceBz,
    readResourceString,
  ) where

import qualified Control.Monad.State                     as M
import qualified Data.ByteString.Lazy                    as Bz
import qualified System.Directory                        as Dir
import qualified System.FilePath                         as Path
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Resource.Include   as C
import qualified Koshucode.Baala.Core.Resource.Queue     as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Core.Resource.Message   as Msg


-- --------------------------------------------  I/O with global

-- | I/O with global state.
type GlobalIO a c = M.StateT (C.Global c) IO a

-- | Calculation that returns abortable resource.
type ResourceIO c = GlobalIO (C.AbResource c) c

-- | Run state monad with global state.
gio :: C.Global c -> GlobalIO a c -> IO (a, C.Global c)
gio g a = M.runStateT a g

-- | Get root resource.
getRoot :: GlobalIO (C.Resource c) c
getRoot = return . C.globalHook =<< M.get

-- | Get and update 'C.globalSourceCount'.
nextSourceCount :: GlobalIO Int c
nextSourceCount =
    do g <- M.get
       let n = 1 + C.globalSourceCount g
       M.put $ g { C.globalSourceCount = n }
       return n


-- --------------------------------------------  Read resource

-- | Read data resource from lazy bytestring.
readResourceBz :: (D.CContent c) => C.Resource c -> B.Bz -> C.AbResource c
readResourceBz base code = C.resInclude [] "" base (B.nioFrom code) code

-- | Read data resource from text.
readResourceString :: (D.CContent c) => C.Resource c -> String -> C.AbResource c
readResourceString base code = readResourceBz base $ B.stringBz code

-- | Read data resource from single input point.
readResourceSingle :: (D.CContent c) => C.Global c -> B.IOPoint -> IO (C.AbResource c, C.Global c)
readResourceSingle g src = gio g single where
    single = do root <- getRoot
                readResourceLimit 1 root [src]

-- | Read data resource from multiple input points.
readResource :: (D.CContent c) => C.Global c -> [B.IOPoint] -> IO (C.AbResource c, C.Global c)
readResource g src = gio g proc where
    proc = do root <- getRoot
              let limit = C.globalSourceLimit $ C.getGlobal root
              readResourceLimit limit root src

readResourceLimit :: (D.CContent c) => Int -> C.Resource c -> [B.IOPoint] -> ResourceIO c
readResourceLimit limit root src =
    readQueue limit $ root { C.resInputQueue = (C.qFrom ready, []) }
    where ready = map input $ reverse src
          input pt = C.InputPoint pt []

-- | Read input at most given limit on the queue.
readQueue :: (D.CContent c) => Int -> C.Resource c -> ResourceIO c
readQueue limit res@C.Resource { C.resInputQueue = (q, done) }
    | limit <= 0  = return $ Right res
    | otherwise   = proc $ C.deq q
    where
      proc (Nothing, _) = return $ Right res
      proc (Just src, q')
          | B.NIOPoint 0 srcPt `elem` done = readQueue limit' pop  -- skip
          | otherwise                      = readOne
          where
            limit'   = limit - 1
            srcPt    = C.inputPoint src
            srcAbout = C.inputPointAbout src
            pop      = res { C.resInputQueue = (q', done) }
            readOne  = do n <- nextSourceCount
                          let src' = B.NIOPoint n srcPt
                          abres' <- readCode pop src' srcAbout
                          case abres' of
                            Right res' -> readQueue limit' $ C.resQueueDone src' res'
                            left       -> return left

-- | Read resource from certain source.
readCode :: forall c. (D.CContent c) =>
    C.Resource c -> B.NIOPoint -> [S.TTree] -> ResourceIO c
readCode res src add = dispatch $ B.nioPoint src where
    dispatch (B.IOPointFile cd path) = M.liftIO $ do
         let path' = putDir cd path
             cd'   = putDir cd $ Path.dropFileName path
         exist <- Dir.doesFileExist path'
         case exist of
           True   -> includeUnder cd' =<< Bz.readFile path'
           False  -> return $ Msg.noFile cd path

    dispatch (B.IOPointUri url) =
        do g <- M.get
           let proxy = C.globalProxy g
           abcode <- M.liftIO $ B.uriContent proxy url
           M.liftIO $ case abcode of
             Right code       -> include code
             Left (code, msg) -> return $ Msg.httpError url code msg

    dispatch (B.IOPointText   _ code) = M.liftIO $ include code
    dispatch (B.IOPointCustom _ code) = M.liftIO $ include code
    dispatch (B.IOPointStdin)         = lift $ include =<< Bz.getContents
    dispatch _                        = B.bug "read resource"

    putDir dir path  = cutDot dir ++ cutDot path

    cutDot ('.' : '/' : path) = cutDot path
    cutDot path               = path

    include :: B.Bz -> IO (C.AbResource c)
    include = includeUnder ""

    includeUnder :: FilePath -> B.Bz -> IO (C.AbResource c)
    includeUnder cd = return . C.resInclude add' cd res src

    add' = concatMap B.untree add

    lift :: IO a -> GlobalIO a c
    lift = M.liftIO


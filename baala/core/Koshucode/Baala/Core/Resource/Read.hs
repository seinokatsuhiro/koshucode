{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Read data resource.

module Koshucode.Baala.Core.Resource.Read
  ( -- * Read resource
    resRead,
    resReadSingle,
    resReadBz,
    resReadString,
  ) where

import qualified Control.Monad.State                     as M
import qualified Data.ByteString.Lazy                    as Bz
import qualified System.Directory                        as Dir
import qualified System.FilePath                         as Path
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Resource.Include   as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Core.Resource.Message   as Msg


-- --------------------------------------------  I/O with global

-- | I/O with global state.
type GlobalIO a c = M.StateT (C.Global c) IO a

-- | Calculation that returns abortable resource.
type ResourceIO c = GlobalIO (C.AbResource c) c

-- | Run state monad with global state.
globalIO :: (C.Resource c -> GlobalIO a c) -> C.Global c -> IO (a, C.Global c)
globalIO f g =
    let g' = setGlobal g
    in M.runStateT (f $ C.globalHook g') g'

-- | Set global data to root resource.
setGlobal :: O.Map (C.Global c)
setGlobal g =
    let root = C.globalHook g
    in  g { C.globalHook = root { C.resGlobal = g }}

-- | Get and update 'C.globalSourceCount'.
nextSourceCount :: GlobalIO Int c
nextSourceCount =
    do g <- M.get
       let n = 1 + C.globalSourceCount g
       M.put $ g { C.globalSourceCount = n }
       return n


-- --------------------------------------------  Read resource

-- | Read data resource from lazy bytestring.
resReadBz :: (D.CContent c) => C.Resource c -> B.Bz -> C.AbResource c
resReadBz base code = C.resInclude [] "" base (B.codeIxIO code) code

-- | Read data resource from text.
resReadString :: (D.CContent c) => C.Resource c -> String -> C.AbResource c
resReadString base code = resReadBz base $ B.stringBz code

-- | Read data resource from single input point.
resReadSingle :: (D.CContent c) => C.Global c -> B.IOPoint -> IO (C.AbResource c, C.Global c)
resReadSingle g src = globalIO single g where
    single root = resReadLimit 1 root [src]

-- | Read data resource from multiple input points.
resRead :: (D.CContent c) => C.Global c -> [B.IOPoint] -> IO (C.AbResource c, C.Global c)
resRead g src = globalIO proc g where
    proc root = do let limit = C.globalSourceLimit g
                   resReadLimit limit root src

resReadLimit :: (D.CContent c) => Int -> C.Resource c -> [B.IOPoint] -> ResourceIO c
resReadLimit limit root src =
    readQueue limit $ root { C.resInputQueue = (B.qFrom ready, []) }
    where ready = map input $ reverse src
          input pt = C.InputPoint pt []

-- | Read input at most given limit on the queue.
readQueue :: (D.CContent c) => Int -> C.Resource c -> ResourceIO c
readQueue limit res@C.Resource { C.resInputQueue = (q, done) }
    | limit <= 0  = return $ Right res
    | otherwise   = proc $ B.deq q
    where
      proc (Nothing, _) = return $ Right res
      proc (Just src, q')
          | B.IxIOPoint 0 srcPt `elem` done = readQueue limit' pop  -- skip
          | otherwise                       = readOne
          where
            limit'   = limit - 1
            srcPt    = C.inputPoint src
            srcAbout = C.inputPointAbout src
            pop      = res { C.resInputQueue = (q', done) }
            readOne  = do n <- nextSourceCount
                          let src' = B.IxIOPoint n srcPt
                          abres' <- readCode pop src' srcAbout
                          case abres' of
                            Right res' -> readQueue limit' $ C.resQueueDone src' res'
                            left       -> return left

-- | Read resource from certain source.
readCode :: forall c. (D.CContent c) =>
    C.Resource c -> B.IxIOPoint -> [S.Tree] -> ResourceIO c
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
           abcode <- M.liftIO $ B.httpGet proxy url
           M.liftIO $ case abcode of
             Right code       -> include code
             Left (code, msg) -> return $ Msg.httpError url code msg

    dispatch (B.IOPointText   _ code) = M.liftIO $ include code
    dispatch (B.IOPointCustom _ code) = M.liftIO $ include code
    dispatch (B.IOPointStdin _)       = lift $ include =<< Bz.getContents
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


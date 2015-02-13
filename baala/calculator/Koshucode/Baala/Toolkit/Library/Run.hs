{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Toolkit.Library.Run
  ( runFiles, hRunFiles,
    --runCalc, runCalcTo,
    theContent,
    --mkdir,
  ) where

import qualified System.IO            as IO
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C



-- ----------------------

runFiles :: (C.CContent c) => C.Global c -> [B.CodeName] -> IO Int
runFiles = hRunFiles IO.stdout

-- | Read and union sections from files, and run the section.
hRunFiles
    :: (C.CContent c)
    => IO.Handle          -- ^ Output file handle
    -> C.Global c         -- ^ Global parameters
    -> [B.CodeName]       -- ^ Names of source codes
    -> IO Int
hRunFiles h g ns =
    do (abRes, _) <- C.gioResource (C.readSources ns) g
       let inputs  = B.codeNameText `map` ns
           comm    = B.CommentDoc [ B.CommentSec "INPUT" inputs ]

       IO.hSetEncoding h IO.utf8
       IO.hPutStrLn    h B.emacsModeComment
       IO.hPutStr      h $ unlines $ B.texts comm
       IO.hPutStrLn    h ""

       let cmd = C.globalCommandLine g
           js' = do res <- abRes
                    C.runResource res

       case js' of
         Left a   -> B.abort cmd a
         Right js -> B.hPutOutputResult h js



-- ---------------------- Calculation list

-- mkdir :: FilePath -> IO ()
-- mkdir path = 
--     do let dir = Path.dropFileName path
--        Dir.createDirectoryIfMissing True dir



-- ----------------------  The

theContent :: (C.CContent c) => String -> [B.Named c] -> Maybe c
theContent = lookup

-- theContents :: (C.CContent c) => [String] -> [B.Named c] -> Maybe [c]
-- theContents ns assn = mapM (`theContent` assn) ns

-- theStrings :: (C.CContent c) => c -> [String]
-- theStrings c | C.isText c  =  [C.gText c]
-- theStrings c | C.isList c  =  map C.gText $ C.gList c
-- theStrings _               =  []


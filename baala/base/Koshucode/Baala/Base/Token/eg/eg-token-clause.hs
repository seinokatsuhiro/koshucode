#!/usr/bin/env koshu-env.sh runhaskell
{-# OPTIONS_GHC -Wall #-}
-- --------------------------------------------
--
--  DESCRIPTION
--    Examples of clausify
--
--  USAGE
--    chmod 755 eg-token-clause.hs
--    ./eg-token-clause.hs
--    ./eg-token-clause.hs 3
---
-- --------------------------------------------

import qualified System.Environment as Sys
import qualified Koshucode.Baala.Base as B

main :: IO ()
main =
    do args <- Sys.getArgs
       case args of
         []    -> egClausify eg
         (n:_) -> egClausify [eg !! (read n :: Int)]

egClausify :: [(Int, String)] -> IO ()
egClausify xs = mapM put xs >> return ()

put :: (Int, String) -> IO ()
put (n, line) =
    do let dline = B.docv . B.tokenClauses . B.tokenLines $ line
       putStrLn $ show $ B.docHang (B.doc n) 2 dline
       putStrLn ""

eg :: [(Int, String)]
eg = zip [0 ..]
     [ ""

     -- 1 行が 1 節になる
     , "a x\nb y\nc z\n\n"

     -- 継続行を認識する
     , "a x\n b y\nc z\n"

     -- 複数の継続行を認識する
     , "a x\n b y\n c z\n"

     -- 字下げ数の異なる継続行を認識する
     , "a x\n  b y\n c z\n"

     -- 初回の字下げを継続する
     , " a x\n b y\nc z\n"

     -- 初回の字下げを継続し，継続行を認識する
     , " a x\n  b y\nc z\n"

     -- 空行を読み飛ばす
     , "a x\n\n b y\nc z\n"

     -- 空白を含む空行を読み飛ばす
     , "a x\n\n  \n b y\nc z\n"
     ]


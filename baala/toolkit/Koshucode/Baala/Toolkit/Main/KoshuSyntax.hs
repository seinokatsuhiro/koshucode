{-# OPTIONS_GHC -Wall #-}

{-| Koshucode syntactic tool. -}

module Koshucode.Baala.Toolkit.Main.KoshuSyntax
( koshuSyntaxMain

  -- * koshu-syntax.hs
  -- $koshu-syntax.hs
) where

import Control.Monad
import System.Console.GetOpt

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Vanilla as V

import qualified Koshucode.Baala.Toolkit.Library.Exit    as L
import qualified Koshucode.Baala.Toolkit.Library.Version as L



-- ----------------------  Option

data Option
    = OptHelp
    | OptVersion
    | OptShowEncoding
    | OptRun
    | OptStdin
    | OptOnlyToken
      deriving (Show, Eq, Ord, Enum, Bounded)

koshuOptions :: [OptDescr Option]
koshuOptions =
    [ Option "h" ["help"]     (NoArg OptHelp)    "Print help message."
    , Option "V" ["version"]  (NoArg OptVersion) "Print version number."
    , Option ""  ["run"]      (NoArg OptRun)     "Run section."
    , Option ""  ["show-encoding"] (NoArg OptShowEncoding) "Show character encoding."
    , Option "i" ["stdin"]    (NoArg OptStdin)  "Read from stdin."
    , Option "t" ["token"]    (NoArg OptOnlyToken) "Show only tokens."
    ]

version :: String
version = "koshu-syntax-" ++ L.versionString

usage :: String
usage = usageInfo header koshuOptions

header :: String
header = unlines
    [ "DESCRIPTION"
    , "  Syntaxtic tool of Koshucode."
    , ""
    ] ++ "OPTIONS"



-- ----------------------  Main

{-| The main function for @koshu-syntax@ command. -}
koshuSyntaxMain :: IO ()
koshuSyntaxMain = koshuSyntaxMain' =<< L.prelude

koshuSyntaxMain' :: (String, [String]) -> IO ()
koshuSyntaxMain' (_, argv) =
    case getOpt Permute koshuOptions argv of
      (opts, files, [])
          | has OptHelp         -> L.putSuccess usage
          | has OptVersion      -> L.putSuccess $ version ++ "\n"
          | has OptShowEncoding -> L.putSuccess =<< L.currentEncodings
          | has OptOnlyToken    -> mapM_ dumpToken files
          | otherwise           -> mapM_ dumpClauseAndToken files
          where has = (`elem` opts)
      (_, _, errs) -> L.putFailure $ concat errs



-- ----------------------  Dump clauses and tokens

dumpClauseAndToken :: FilePath -> IO ()
dumpClauseAndToken path = 
    do code <- readFile path
       let ts = B.tokenLines (B.ResourceFile path) code
           cs = C.consPreclause ts
       putStr $ unlines h
       mapM_ putClause $ zip [1 ..] cs
    where
      h = [ "***"
          , "***  DESCRIPTION"
          , "***    Clauses and tokens from " ++ show path
          , "***"
          , "***  LEGEND"
          , "***    *** [C0] clause type"
          , "***    *** [C0] L0 line content"
          , "***    |-- CLAUSE /clause-seq /clause-type"
          , "***    |-- TOKEN /token-seq /token-type /token-content"
          , "***" ]

putClause :: (Int, C.Clause) -> IO ()
putClause p@(cn, c) =
  do putStrLn ""
     putStrLn $ "*** [C" ++ show cn ++ "] " ++ C.clauseTypeText c

     print $ B.doc $ clauseJudge p
     let src = C.clauseSource c
         ls  = B.clauseLines src
     foldM_ (putToken cn) 1 ls

clauseJudge :: (Int, C.Clause) -> B.Judge V.VContent
clauseJudge (cn, c) = B.Judge True "CLAUSE" args where
    args = [ ("/clause-seq"  , C.putDecFromInt cn)
           , ("/clause-type" , C.putText $ C.clauseTypeText c)]

putToken :: Int -> Int -> B.TokenLine -> IO (Int)
putToken cn tn (B.CodeLine ln line toks) =
  do putStrLn ""
     putStrLn $ "*** [C" ++ show cn ++ "] L" ++ show ln ++ " " ++ show line
     print $ B.docv $ map (tokenJudge cn) toks
     return $ tn + length toks

tokenJudge :: Int -> B.Token -> B.Judge V.VContent
tokenJudge cn t = B.Judge True "TOKEN" xs where
    xs = [ ("/clause-seq"   , C.putDecFromInt cn)
         , ("/token-type"   , C.putText $ B.tokenTypeText t)
         , ("/token-content", C.putText $ B.tokenContent t) ]



-- ----------------------  Dump only tokens, not clauses

dumpToken :: FilePath -> IO ()
dumpToken path =
    do code <- readFile path
       let xs = B.tokenLines (B.ResourceFile path) code
           (_, ls) = foldl dumpTokenText (0, []) xs
       putStr $ unlines ls

dumpTokenText :: (Int, [String]) -> B.TokenLine -> (Int, [String])
dumpTokenText (n, ys) (B.CodeLine l line ts) = (n + length ts, ys ++ xs) where
    h  = ["", "**  L" ++ show l ++ " " ++ show line]
    xs = h ++ (map dump ts)
    dump p = show $ B.doc $ dumpTokenJudge l p

dumpTokenJudge :: Int -> B.Token -> B.Judge V.VContent
dumpTokenJudge l t = B.Judge True "TOKEN" xs where
    xs = [ ("/line"         , C.putDecFromInt l)
         , ("/token-type"   , C.putText $ B.tokenTypeText t)
         , ("/token-content", C.putText $ B.tokenContent  t) ]



-- ----------------------
{- $koshu-syntax.hs

   @koshu-syntax@ command is implemented using 'koshuSyntaxMain'.

   > import Koshucode.Baala.Toolkit.Main.KoshuSyntaxMain
   > 
   > main :: IO ()
   > main = koshuSyntaxMain
-}


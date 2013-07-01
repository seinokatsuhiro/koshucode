{-# OPTIONS_GHC -Wall #-}

{-| Koshucode syntactic tool. -}

module Koshucode.Baala.Toolkit.Main.KoshuSyntax
( koshuSyntaxMain

-- * koshu-syntax.hs
-- $koshu-syntax.hs
) where

import Control.Monad
import System.Console.GetOpt

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Section as Sec
import Koshucode.Baala.Base.Syntax  as Syn
import Koshucode.Baala.Vanilla

import Koshucode.Baala.Toolkit.Library.Exit
import Koshucode.Baala.Toolkit.Library.Version



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
version = "koshu-syntax-" ++ versionString

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
koshuSyntaxMain = koshuSyntaxMain' =<< prelude

koshuSyntaxMain' :: (String, [String]) -> IO ()
koshuSyntaxMain' (_, argv) =
    case getOpt Permute koshuOptions argv of
      (opts, files, [])
          | has OptHelp         -> putSuccess usage
          | has OptVersion      -> putSuccess $ version ++ "\n"
          | has OptShowEncoding -> putSuccess =<< currentEncodings
          | has OptOnlyToken    -> mapM_ dumpToken files
          | otherwise           -> mapM_ dumpClauseAndToken files
          where has = (`elem` opts)
      (_, _, errs) -> putFailure $ concat errs



-- ----------------------  Dump clauses and tokens

dumpClauseAndToken :: FilePath -> IO ()
dumpClauseAndToken path = 
    do code <- readFile path
       let ts = Syn.sourceLines code
           cs = Sec.consPreclause ts
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

putClause :: (Int, Clause) -> IO ()
putClause p@(cn, c) =
  do putStrLn ""
     putStrLn $ "*** [C" ++ show cn ++ "] " ++ clauseTypeText c

     print $ doc $ clauseJudge p
     let src = clauseSource c
         ls  = clauseLines src
     foldM_ (putToken cn) 1 ls

clauseJudge :: (Int, Clause) -> Judge Val
clauseJudge (cn, c) = Judge True "CLAUSE" args where
    args = [ ("/clause-seq"  , intv cn)
           , ("/clause-type" , stringv $ clauseTypeText c)]

putToken :: Int -> Int -> SourceLine -> IO (Int)
putToken cn tn (SourceLine ln line toks) =
  do putStrLn ""
     putStrLn $ "*** [C" ++ show cn ++ "] L" ++ show ln ++ " " ++ show line
     print $ docv $ map (tokenJudge cn) $ zip [tn..] toks
     return $ tn + length toks

tokenJudge :: Int -> (Int, Token) -> Judge Val
tokenJudge cn (n, t) = Judge True "TOKEN" xs where
    xs = [ ("/clause-seq"   , intv cn)
         , ("/token-seq"    , intv n)
         , ("/token-type"   , stringv $ Syn.tokenTypeText t)
         , ("/token-content", stringv $ Syn.tokenContent t) ]



-- ----------------------  Dump only tokens, not clauses

dumpToken :: FilePath -> IO ()
dumpToken path =
    do code <- readFile path
       let xs = Syn.sourceLines code
           (_, ls) = foldl dumpTokenText (0, []) xs
       putStr $ unlines ls

dumpTokenText :: (Int, [String]) -> SourceLine -> (Int, [String])
dumpTokenText (n, ys) (SourceLine l line ts) = (n + length ts, ys ++ xs) where
    h  = ["", "**  L" ++ show l ++ " " ++ show line]
    xs = h ++ (map dump $ zip [n..] ts)
    dump p = show $ doc $ dumpTokenJudge l p

dumpTokenJudge :: Int -> (Int, Token) -> Judge Val
dumpTokenJudge l (n, t) = Judge True "TOKEN" xs where
    xs = [ ("/line"         , intv l)
         , ("/token-seq"    , intv n)
         , ("/token-type"   , stringv $ Syn.tokenTypeText t)
         , ("/token-content", stringv $ Syn.tokenContent  t) ]



-- ----------------------
-- $koshu-syntax.hs
--
-- @koshu-syntax@ command is implemented using 'koshuSyntaxMain'.
--
-- @
-- import Koshucode.Baala.Toolkit.Main.KoshuSyntaxMain
-- @
--
-- @
-- main :: IO ()
-- main = koshuSyntaxMain
-- @


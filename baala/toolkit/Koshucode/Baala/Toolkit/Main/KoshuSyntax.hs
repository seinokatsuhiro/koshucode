{-# OPTIONS_GHC -Wall #-}

-- | Koshucode syntactic tool.

module Koshucode.Baala.Toolkit.Main.KoshuSyntax
( koshuSyntaxMain
  -- * koshu-syntax.hs
  -- $koshu-syntax.hs
) where

import qualified Control.Monad                  as M
import qualified System.Console.GetOpt          as G
import qualified Koshucode.Baala.Base           as B
import qualified Koshucode.Baala.Core           as C
import qualified Koshucode.Baala.Type.Vanilla   as Type

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

koshuSyntaxOptions :: [G.OptDescr Option]
koshuSyntaxOptions =
    [ G.Option "h" ["help"]     (G.NoArg OptHelp)    "Print help message."
    , G.Option "V" ["version"]  (G.NoArg OptVersion) "Print version number."
    , G.Option ""  ["run"]      (G.NoArg OptRun)     "Run section."
    , G.Option ""  ["show-encoding"] (G.NoArg OptShowEncoding) "Show character encoding."
    , G.Option "i" ["stdin"]    (G.NoArg OptStdin)  "Read from stdin."
    , G.Option "t" ["token"]    (G.NoArg OptOnlyToken) "Show only tokens."
    ]

version :: String
version = "koshu-syntax-" ++ L.versionString

usage :: String
usage = G.usageInfo header koshuSyntaxOptions

header :: String
header = unlines
    [ "DESCRIPTION"
    , "  Syntaxtic tool of Koshucode."
    , ""
    ] ++ "OPTIONS"



-- ----------------------  Main

-- | The main function for @koshu-syntax@ command.
koshuSyntaxMain :: IO ()
koshuSyntaxMain = koshuSyntaxMain' =<< L.prelude

koshuSyntaxMain' :: (String, [String]) -> IO ()
koshuSyntaxMain' (_, argv) =
    case G.getOpt G.Permute koshuSyntaxOptions argv of
      (opts, files, [])
          | has OptHelp          ->  L.putSuccess usage
          | has OptVersion       ->  L.putSuccess $ version ++ "\n"
          | has OptShowEncoding  ->  L.putSuccess =<< L.currentEncodings
          | has OptOnlyToken     ->  mapM_ dumpToken files
          | otherwise            ->  mapM_ dumpClauseAndToken files
          where has = (`elem` opts)
      (_, _, errs) -> L.putFailure $ concat errs



-- ----------------------  Print judges

putJudges :: (Ord c, B.Write c) => [B.Judge c] -> IO ()
putJudges = mapM_ putJudge

putJudge :: (Ord c, B.Write c) => B.Judge c -> IO ()
putJudge = putStrLn . judgeText

judgeText :: (Ord c, B.Write c) => B.Judge c -> String
judgeText = show . B.write B.shortEmpty


-- ----------------------  Dump clauses and tokens

dumpClauseAndToken :: FilePath -> IO ()
dumpClauseAndToken path = 
    do code <- readFile path
       let ts = B.tokenLines (B.ResourceFile path) code
           cs = C.consPreclause ts
       B.putLines $ B.texts h
       mapM_ putClause $ zip [1 ..] cs
    where
      h = B.CommentDoc
          [ B.CommentSec "DESCRIPTION"
            [ "Clauses and tokens from " ++ show path ]
          , B.CommentSec "LEGEND"
            [ "*** [C0] clause type"
            , "*** [C0] L0 line content"                       
            , "|-- CLAUSE /clause /clause-type"
            , "|-- TOKEN /clause /line /seq /type /cont" ]]

putClause :: (Int, C.Clause) -> IO ()
putClause p@(clseq, c) =
  do putStrLn ""
     putStrLn $ "*** [C" ++ show clseq ++ "] " ++ C.clauseTypeText c
     putJudge $ clauseJudge p
     let src = C.clauseSource c
         ls  = B.clauseLines src
     M.foldM_ (putToken clseq) 1 ls

clauseJudge :: (Int, C.Clause) -> B.Judge Type.VContent
clauseJudge (clseq, c) = B.affirm "CLAUSE" args where
    args = [ ("clause"      , C.pDecFromInt clseq)
           , ("clause-type" , C.pText $ C.clauseTypeText c)]

putToken :: Int -> Int -> B.TokenLine -> IO Int
putToken clseq tn (B.CodeLine ln line toks) =
  do putStrLn ""
     putStrLn  $ "*** [C" ++ show clseq ++ "] L" ++ show ln ++ " " ++ show line
     putJudges $ map (tokenJudge clseq ln) toks
     return    $ tn + length toks

tokenJudge :: Int -> Int -> B.Token -> B.Judge Type.VContent
tokenJudge clseq ln tok
    = B.affirm "TOKEN"
      [ ("clause" , C.pDecFromInt clseq)
      , ("line"   , C.pDecFromInt ln)
      , ("col"    , C.pDecFromInt $ B.codeColumnNumber $ head $ B.codePts tok)
      , ("type"   , C.pText $ B.tokenTypeText tok)
      , ("cont"   , C.pText $ B.tokenContent  tok) ]



-- ----------------------  Dump only tokens, not clauses

dumpToken :: FilePath -> IO ()
dumpToken path =
    do code <- readFile path
       let xs = B.tokenLines (B.ResourceFile path) code
           (_, ls) = foldl dumpTokenText (0, []) xs
       B.putLines ls

dumpTokenText :: (Int, [String]) -> B.TokenLine -> (Int, [String])
dumpTokenText (n, ys) (B.CodeLine l line ts) = (n + length ts, ys ++ xs) where
    h    = ["", "**  L" ++ show l ++ " " ++ show line]
    xs   = h ++ map dump ts
    dump = judgeText . dumpTokenJudge l

dumpTokenJudge :: Int -> B.Token -> B.Judge Type.VContent
dumpTokenJudge l t = B.affirm "TOKEN" xs where
    xs = [ ("line"   , C.pDecFromInt l)
         , ("type"   , C.pText $ B.tokenTypeText t)
         , ("cont"   , C.pText $ B.tokenContent  t) ]



-- ----------------------
-- $koshu-syntax.hs
--
-- @koshu-syntax@ command is implemented using 'koshuSyntaxMain'.
--
-- > import Koshucode.Baala.Toolkit.Main.KoshuSyntaxMain
-- > 
-- > main :: IO ()
-- > main = koshuSyntaxMain
--


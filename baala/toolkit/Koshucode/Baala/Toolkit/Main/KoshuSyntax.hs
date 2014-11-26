{-# OPTIONS_GHC -Wall #-}

-- | Koshucode syntactic tool.

module Koshucode.Baala.Toolkit.Main.KoshuSyntax
  ( koshuSyntaxMain
    -- * koshu-syntax.hs
    -- $koshu-syntax.hs
  ) where

import qualified System.Console.GetOpt                   as G
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Core                    as C
import qualified Koshucode.Baala.Type.Vanilla            as Type
import qualified Koshucode.Baala.Toolkit.Library.Exit    as L
import qualified Koshucode.Baala.Toolkit.Library.Version as L


-- ----------------------  Option

data Option
    = OptHelp
    | OptVersion
    | OptShowEncoding
    | OptStdin
    | OptOmitBlank
      deriving (Show, Eq, Ord, Enum, Bounded)

koshuSyntaxOptions :: [G.OptDescr Option]
koshuSyntaxOptions =
    [ G.Option "h" ["help"]          (G.NoArg OptHelp)         "Print help message."
    , G.Option "V" ["version"]       (G.NoArg OptVersion)      "Print version number."
    , G.Option ""  ["show-encoding"] (G.NoArg OptShowEncoding) "Show character encoding."
    , G.Option "i" ["stdin"]         (G.NoArg OptStdin)        "Read from stdin."
    , G.Option "b" ["omit-blank"]    (G.NoArg OptOmitBlank)    "Omit space and comment tokens."
    ]

version :: String
version = "koshu-syntax-" ++ L.versionString

usage :: String
usage = G.usageInfo usageDesc koshuSyntaxOptions

usageDesc :: String
usageDesc = unlines
    [ "DESCRIPTION"
    , "  Syntaxtic tool of Koshucode."
    , ""
    , "USAGE"
    , "  koshu-syntax [OPTIONS ...] FILE.k    # single file"
    , "  koshu-syntax [OPTIONS ...] -i        # standard input"
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
          | has OptHelp          -> L.putSuccess usage
          | has OptVersion       -> L.putSuccess $ version ++ "\n"
          | has OptShowEncoding  -> L.putSuccess =<< L.currentEncodings
          | has OptStdin         -> dumpStdin omit
          | length files == 1    -> dumpFile omit $ head files
          | otherwise            -> L.putFailure usage
          where has     = (`elem` opts)
                omit    = has OptOmitBlank

      (_, _, errs)    -> L.putFailure $ concat errs


-- ----------------------  Judges

description :: FilePath -> B.CommentDoc
description path = B.CommentDoc [desc, input, js] where
    desc   = B.CommentSec "DESCRIPTION" [ "Clauses and tokens" ]
    input  = B.CommentSec "INPUT" [ path ]
    js     = B.CommentSec "JUDGES"
             [ "|-- CLAUSE /clause /clause-type"
             , "|-- LINE   /clause /line"
             , "|-- TOKEN  /line /column /token-type /cont"
             , ""
             , "<<< There is a clause numbered /clause on /line ."
             , "    Type of the clause is /clause-type ."
             , "    There is a token of content /cont at /line and /column ."
             , "    Type of the token is /token-type . >>>" ]

judgeClause :: Int -> C.Clause -> B.Judge Type.VContent
judgeClause clseq c = B.affirm "CLAUSE" args where
    args = [ ("clause"       , C.pDecFromInt clseq)
           , ("clause-type"  , C.pText $ C.clauseTypeText c)]

judgeLine :: Int -> B.TokenLine -> B.Judge Type.VContent
judgeLine clseq (B.CodeLine ln _ _) = B.affirm "LINE" args where
    args = [ ("clause"       , C.pDecFromInt clseq)
           , ("line"         , C.pDecFromInt ln) ]

judgeToken :: Int -> B.Token -> B.Judge Type.VContent
judgeToken ln tok = B.affirm "TOKEN" args where
    args = [ ("line"         , C.pDecFromInt ln)
           , ("column"       , C.pDecFromInt $ B.codeColumnNumber $ head $ B.codePts tok)
           , ("token-type"   , C.pText $ B.tokenTypeText tok)
           , ("cont"         , C.pText $ B.tokenContent  tok) ]


-- ----------------------  Print judges

putJudges :: (Ord c, B.Write c) => [B.Judge c] -> IO ()
putJudges = mapM_ putJudge

putJudge :: (Ord c, B.Write c) => B.Judge c -> IO ()
putJudge = putStrLn . judgeText

judgeText :: (Ord c, B.Write c) => B.Judge c -> String
judgeText = show . B.write B.shortEmpty

putNewline :: IO ()
putNewline = putStrLn ""

ab :: (a -> IO b) -> B.Ab a -> IO b
ab _ (Left a)   = B.abort [] a
ab f (Right b)  = f b


-- ----------------------  Dump

dumpFile :: Bool -> FilePath -> IO ()
dumpFile omit path = dumpCode omit path =<< readFile path

dumpStdin :: Bool -> IO ()
dumpStdin omit = dumpCode omit "(stdin)" =<< getContents

dumpCode :: Bool -> FilePath -> String -> IO ()
dumpCode omit path code = 
    ab f $ B.tokenLines (B.Source 0 $ B.SourceFile path) code
    where f ts = do let cs = C.consPreclause ts
                    B.putLines $ B.texts $ description path
                    dumpClause omit `mapM_` zip [1 ..] cs
                    putNewline

dumpClause :: Bool -> (Int, C.Clause) -> IO ()
dumpClause omit (clseq, c) =
    do let src = C.clauseSource c
           ls  = B.clauseLines src
       putNewline
       B.putLines $ map comment ls
       putNewline
       putJudge $ judgeClause clseq c
       dumpLine clseq ls
       dumpToken omit `mapM_` ls
    where
      comment line = "**  " ++ B.lineContent line

dumpLine :: Int -> [B.TokenLine] -> IO ()
dumpLine clseq ls = putJudges $ map (judgeLine clseq) ls

dumpToken :: Bool -> B.TokenLine -> IO ()
dumpToken omit (B.CodeLine ln _ toks) =
    do putNewline
       putJudges $ map (judgeToken ln) toks'
    where
      toks' | omit       = B.omit B.isBlankToken toks
            | otherwise  = toks


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


{-# OPTIONS_GHC -Wall #-}

-- | Koshucode syntactic tool.

module Koshucode.Baala.Toolkit.Main.KoshuSyntax
  ( koshuSyntaxMain
    -- * koshu-syntax.hs
    -- $koshu-syntax.hs
  ) where

import qualified System.Console.GetOpt                   as G
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core                    as C
import qualified Koshucode.Baala.Toolkit.Library.Exit    as L
import qualified Koshucode.Baala.Toolkit.Library.Version as L


-- ----------------------  Option

data Option
    = OptHelp
    | OptVersion
    | OptStdin
    | OptOmitBlank
    | OptDict
    | OptEncoding
      deriving (Show, Eq, Ord, Enum, Bounded)

koshuSyntaxOptions :: [G.OptDescr Option]
koshuSyntaxOptions =
    [ G.Option "h" ["help"]        (G.NoArg OptHelp)        "Print help message."
    , G.Option "V" ["version"]     (G.NoArg OptVersion)     "Print version number."
    , G.Option "i" ["stdin"]       (G.NoArg OptStdin)       "Read from stdin."
    , G.Option "b" ["omit-blank"]  (G.NoArg OptOmitBlank)   "Omit space and comment tokens."
    , G.Option ""  ["dict"]        (G.NoArg OptDict)        "Show dictionary."
    , G.Option ""  ["encoding"]    (G.NoArg OptEncoding)    "Show character encoding."
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
          | has OptDict          -> dumpDict
          | has OptEncoding      -> L.putSuccess =<< L.currentEncodings
          | has OptStdin         -> dumpStdin omit
          | length files == 1    -> dumpFile omit $ head files
          | otherwise            -> L.putFailure usage
          where has     = (`elem` opts)
                omit    = has OptOmitBlank

      (_, _, errs)    -> L.putFailure $ concat errs


-- ----------------------  Dump

dumpDesc :: FilePath -> B.CommentDoc
dumpDesc path = B.CommentDoc [desc, input, js] where
    desc   = B.CommentSec "DESCRIPTION" [ "Clauses and tokens" ]
    input  = B.CommentSec "INPUT" [ path ]
    js     = B.CommentSec "JUDGES"
             [ "|-- CLAUSE  /clause -> /clause-type"
             , "|-- LINE    /line -> /clause"
             , "|-- TOKEN   /line /column -> /token-type [/token-subtype] /cont"
             , ""
             , "<<< There is a clause numbered /clause on /line ."
             , "    Type of the clause is /clause-type ."
             , "    There is a token of content /cont at /line and /column ."
             , "    Type of the token is /token-type ."
             , "    Some tokens are classified into /token-subtype . >>>" ]

judgeClause :: Int -> C.Clause -> C.JudgeC
judgeClause clseq c = D.affirm "CLAUSE" args where
    args = [ ("clause"       , D.pInt clseq)
           , ("clause-type"  , D.pText $ C.clauseTypeText c)]

judgeLine :: Int -> D.TokenLine -> C.JudgeC
judgeLine clseq (B.CodeLine ln _ _) = D.affirm "LINE" args where
    args = [ ("line"         , D.pInt ln)
           , ("clause"       , D.pInt clseq) ]

judgeToken :: Int -> D.Token -> C.JudgeC
judgeToken ln tok = D.affirm "TOKEN" $ D.omitEmpty args where
    args = [ ("line"           , D.pInt ln)
           , ("column"         , D.pInt $ B.codePtColumnNo $ head $ B.codePtList tok)
           , ("token-type"     , D.pText $ D.tokenTypeText tok)
           , ("token-subtype"  , D.maybeEmpty D.pText $ D.tokenSubtypeText tok)
           , ("cont"           , D.pText $ D.tokenContent  tok) ]

dumpFile :: Bool -> FilePath -> IO ()
dumpFile omit path = dumpCode omit path =<< readFile path

dumpStdin :: Bool -> IO ()
dumpStdin omit = dumpCode omit "(stdin)" =<< getContents

dumpCode :: Bool -> FilePath -> String -> IO ()
dumpCode omit path code = 
    ab f $ D.tokenLines (B.CodePiece 0 $ B.IOPointFile "" path) code
    where f ts = do let cs = C.consClause [] 0 ts
                    B.putLines $ B.texts $ dumpDesc path
                    dumpClause omit `mapM_` zip [1 ..] cs
                    putNewline

dumpClause :: Bool -> (Int, B.Ab C.Clause) -> IO ()
dumpClause _ (_, Left a) =
    do putNewline
       B.abortPrint [] a
dumpClause omit (clseq, Right c) =
    do let src = C.clauseSource $ C.clauseHead c
           ls  = B.clauseLines src
       putNewline
       B.putLines $ map comment ls
       putNewline
       putJudge $ judgeClause clseq c
       dumpLine clseq ls
       dumpToken omit `mapM_` ls
    where
      comment line = "*** " ++ B.lineContent line

dumpLine :: Int -> [D.TokenLine] -> IO ()
dumpLine clseq ls = putJudges $ map (judgeLine clseq) ls

dumpToken :: Bool -> D.TokenLine -> IO ()
dumpToken omit (B.CodeLine ln _ toks) =
    do putNewline
       putJudges $ map (judgeToken ln) toks'
    where
      toks' | omit       = B.omit D.isBlankToken toks
            | otherwise  = toks


-- ----------------------  Dictionary

dumpDict :: IO ()
dumpDict =
    do B.putLines $ B.texts descDict
       putNewline
       putJudges judgesClauseType
       putNewline
       putJudges judgesTokenType
       putNewline

descDict :: B.CommentDoc
descDict = B.CommentDoc [desc, js] where
    desc   = B.CommentSec "DESCRIPTION" [ "Clauses and tokens" ]
    js     = B.CommentSec "JUDGES"
             [ "|-- CLAUSE-TYPE  /clause-type"
             , "|-- TOKEN-TYPE   /token-type"
             , ""
             , "<<< /clause-type is one of cluase types."
             , "    /token-type is one of token types. >>>" ]

judgeClauseType :: C.Clause -> C.JudgeC
judgeClauseType c = D.affirm "CLAUSE-TYPE" args where
    args = [ ("clause-type", D.pText $ C.clauseTypeText c) ]

judgeTokenType :: D.Token -> C.JudgeC
judgeTokenType t = D.affirm "TOKEN-TYPE" args where
    args = [ ("token-type", D.pText $ D.tokenTypeText t) ]

judgesClauseType :: [C.JudgeC]
judgesClauseType = map j cs where
    j x = judgeClauseType $ C.Clause C.clauseHeadEmpty x
    cs  = [ C.CRelmap "" []
          , C.CAssert D.AssertAffirm "" []
          , C.CJudge  D.AssertAffirm "" []
          , C.CSlot "" []
          ]

judgesTokenType :: [C.JudgeC]
judgesTokenType = map j cs where
    j x = judgeTokenType x
    cs  = [ D.TTextRaw  B.codePtZero ""
          , D.TSlot     B.codePtZero 0 ""
          , D.TTermPath B.codePtZero []
          , D.TOpen     B.codePtZero ""
          , D.TClose    B.codePtZero ""
          , D.TSpace    B.codePtZero 0
          , D.TComment  B.codePtZero ""
          ]


-- ----------------------  Utility

putJudges :: (Ord c, B.Write c) => [D.Judge c] -> IO ()
putJudges = mapM_ putJudge

putJudge :: (Ord c, B.Write c) => D.Judge c -> IO ()
putJudge = putStrLn . D.writeDownJudge B.nullShortener

putNewline :: IO ()
putNewline = putStrLn ""

ab :: (a -> IO b) -> B.Ab a -> IO b
ab _ (Left a)   = B.abort [] a
ab f (Right b)  = f b


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


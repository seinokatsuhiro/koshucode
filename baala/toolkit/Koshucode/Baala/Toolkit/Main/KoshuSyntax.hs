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

judgeClause :: Int -> C.Clause -> B.Judge Type.VContent
judgeClause clseq c = B.affirm "CLAUSE" args where
    args = [ ("clause"       , C.pDecFromInt clseq)
           , ("clause-type"  , C.pText $ C.clauseTypeText c)]

judgeLine :: Int -> B.TokenLine -> B.Judge Type.VContent
judgeLine clseq (B.CodeLine ln _ _) = B.affirm "LINE" args where
    args = [ ("line"         , C.pDecFromInt ln)
           , ("clause"       , C.pDecFromInt clseq) ]

judgeToken :: Int -> B.Token -> B.Judge Type.VContent
judgeToken ln tok = B.affirm "TOKEN" $ C.omitEmpty args where
    args = [ ("line"           , C.pDecFromInt ln)
           , ("column"         , C.pDecFromInt $ B.codePtColumnNo $ head $ B.codePtList tok)
           , ("token-type"     , C.pText $ B.tokenTypeText tok)
           , ("token-subtype"  , C.maybeEmpty C.pText $ B.tokenSubtypeText tok)
           , ("cont"           , C.pText $ B.tokenContent  tok) ]

dumpFile :: Bool -> FilePath -> IO ()
dumpFile omit path = dumpCode omit path =<< readFile path

dumpStdin :: Bool -> IO ()
dumpStdin omit = dumpCode omit "(stdin)" =<< getContents

dumpCode :: Bool -> FilePath -> String -> IO ()
dumpCode omit path code = 
    ab f $ B.tokenLines (B.Source 0 $ B.SourceFile path) code
    where f ts = do let cs = C.consPreclause 0 ts
                    B.putLines $ B.texts $ dumpDesc path
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
      comment line = "*** " ++ B.lineContent line

dumpLine :: Int -> [B.TokenLine] -> IO ()
dumpLine clseq ls = putJudges $ map (judgeLine clseq) ls

dumpToken :: Bool -> B.TokenLine -> IO ()
dumpToken omit (B.CodeLine ln _ toks) =
    do putNewline
       putJudges $ map (judgeToken ln) toks'
    where
      toks' | omit       = B.omit B.isBlankToken toks
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

judgeClauseType :: C.Clause -> B.Judge Type.VContent
judgeClauseType c = B.affirm "CLAUSE-TYPE" args where
    args = [ ("clause-type", C.pText $ C.clauseTypeText c) ]

judgeTokenType :: B.Token -> B.Judge Type.VContent
judgeTokenType t = B.affirm "TOKEN-TYPE" args where
    args = [ ("token-type", C.pText $ B.tokenTypeText t) ]

judgesClauseType :: [B.Judge Type.VContent]
judgesClauseType = map j cs where
    j x = judgeClauseType $ C.Clause (B.CodeClause [] []) 0 x
    cs  = [ C.CShort []
          , C.CRelmap "" []
          , C.CAssert C.AssertAffirm "" [] []
          , C.CJudge  C.AssertAffirm "" []
          , C.CSlot "" []
          , C.CUnknown
          ]

judgesTokenType :: [B.Judge Type.VContent]
judgesTokenType = map j cs where
    j x = judgeTokenType x
    cs  = [ B.TTextRaw B.codePtZero ""
          , B.TSlot    B.codePtZero 0 ""
          , B.TShort   B.codePtZero "" ""
          , B.TTerm    B.codePtZero 0 []
          , B.TOpen    B.codePtZero ""
          , B.TClose   B.codePtZero ""
          , B.TSpace   B.codePtZero 0
          , B.TComment B.codePtZero ""
          ]


-- ----------------------  Utility

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


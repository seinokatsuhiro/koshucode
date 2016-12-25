{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Koshucode syntactic tool.

module Koshucode.Baala.Toolkit.Main.KoshuSyntax
  ( koshuSyntaxMain
    -- * koshu-syntax.hs
    -- $koshu-syntax.hs
  ) where

import qualified System.Console.GetOpt                   as G
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.System                  as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core                    as C
import qualified Koshucode.Baala.Toolkit.Library.Version as L

infixr 0 //
(//) :: n -> c -> (n, c)
(//) n c = (n, c)

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
koshuSyntaxMain = koshuSyntaxMain' =<< B.progAndArgs

koshuSyntaxMain' :: (String, [String]) -> IO ()
koshuSyntaxMain' (_, argv) =
    case G.getOpt G.Permute koshuSyntaxOptions argv of
      (opts, files, [])
          | has OptHelp          -> O.putSuccess usage
          | has OptVersion       -> O.putSuccessLn version
          | has OptDict          -> dumpDict
          | has OptEncoding      -> O.putSuccessLn =<< B.currentEncodings
          | has OptStdin         -> dumpStdin omit
          | length files == 1    -> dumpFile omit $ head files
          | otherwise            -> O.putFailure usage
          where has     = (`elem` opts)
                omit    = has OptOmitBlank

      (_, _, errs)    -> O.putFailure $ concat errs


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
             , "{| There is a clause numbered /clause on /line ."
             , "   Type of the clause is /clause-type ."
             , "   There is a token of content /cont at /line and /column ."
             , "   Type of the token is /token-type ."
             , "   Some tokens are classified into /token-subtype . |}" ]

judgeClause :: Int -> C.Clause -> D.JudgeC
judgeClause clseq c = D.affirm "CLAUSE" args where
    args = [ "clause"       // D.pInt clseq
           , "clause-type"  // D.pText $ C.clauseTypeText c ]

judgeLine :: Int -> S.TokenLine -> D.JudgeC
judgeLine clseq (B.CodeLine ln _ _) = D.affirm "LINE" args where
    args = [ "line"         // D.pInt ln
           , "clause"       // D.pInt clseq ]

judgeToken :: Int -> S.Token -> D.JudgeC
judgeToken ln tok = D.affirm "TOKEN" $ D.cutEmpty args where
    args = [ "line"           // D.pInt ln
           , "column"         // D.pInt $ B.cpColumnNo $ head $ B.getCPs tok
           , "token-type"     // D.pText $ S.subtypeName tok
           , "token-subtype"  // D.maybeEmpty D.pText $ S.tokenDetailTypeString tok
           , "cont"           // D.pText $ S.tokenContent tok ]

dumpFile :: Bool -> FilePath -> IO ()
dumpFile omit path = dumpCode omit path =<< readFile path

dumpStdin :: Bool -> IO ()
dumpStdin omit = dumpCode omit "(stdin)" =<< getContents

dumpCode :: Bool -> FilePath -> String -> IO ()
dumpCode omit path code = 
    f $ S.tokenLines (B.pathIxIO path) code
    where f ts = do let cs = C.consClause [] 0 ts
                    O.putLines $ B.texts $ dumpDesc path
                    dumpClause omit `mapM_` zip [1 ..] cs
                    putNewline

dumpClause :: Bool -> (Int, C.Clause) -> IO ()
dumpClause _ (_, C.Clause _ (C.CUnknown (Left a))) =
    do putNewline
       B.abortPrint [] a
dumpClause omit (clseq, c) =
    do let src = C.clauseSource $ C.clauseHead c
           ls  = B.clauseLines src
       putNewline
       O.putLines $ map comment ls
       putNewline
       putJudge $ judgeClause clseq c
       dumpLine clseq ls
       dumpToken omit `mapM_` ls
    where
      comment line = "*** " ++ B.lineContent line

dumpLine :: Int -> [S.TokenLine] -> IO ()
dumpLine clseq ls = putJudges $ map (judgeLine clseq) ls

dumpToken :: Bool -> S.TokenLine -> IO ()
dumpToken omit (B.CodeLine ln _ toks) =
    do putNewline
       putJudges $ map (judgeToken ln) toks'
    where
      toks' | omit       = B.omit S.isBlankToken toks
            | otherwise  = toks


-- ----------------------  Dictionary

dumpDict :: IO ()
dumpDict =
    do O.putLines $ B.texts descDict
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
             , "{| /clause-type is one of cluase types."
             , "   /token-type is one of token types. |}" ]

judgeClauseType :: C.Clause -> D.JudgeC
judgeClauseType c = D.affirm "CLAUSE-TYPE" args where
    args = [ "clause-type" // D.pText $ C.clauseTypeText c ]

judgeTokenType :: S.Token -> D.JudgeC
judgeTokenType t = D.affirm "TOKEN-TYPE" args where
    args = [ "token-type" // D.pText $ S.subtypeName t ]

judgesClauseType :: [D.JudgeC]
judgesClauseType = map j cs where
    j x = judgeClauseType $ C.Clause B.def x
    cs  = [ C.CRelmap "" []
          , C.CAssert D.AssertAffirm "" []
          , C.CJudge  D.AssertAffirm "" []
          , C.CSlot "" []
          ]

judgesTokenType :: [D.JudgeC]
judgesTokenType = map j cs where
    j x = judgeTokenType x
    cs  = [ S.TText     B.def S.TextRaw ""
          , S.TSlot     B.def 0 ""
          , S.TTerm     B.def ""
          , S.TOpen     B.def ""
          , S.TClose    B.def ""
          , S.TSpace    B.def 0
          , S.TComment  B.def ""
          ]


-- ----------------------  Utility

putJudges :: (B.MixEncode c) => [D.Judge c] -> IO ()
putJudges = mapM_ putJudge

putJudge :: (B.MixEncode a) => a -> IO ()
putJudge = B.putMixLn B.crlfBreak . B.mixEncode

putNewline :: IO ()
putNewline = putStrLn ""


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


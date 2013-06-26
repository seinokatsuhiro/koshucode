{-# OPTIONS_GHC -Wall #-}

{-| Koshucode syntactic tool. -}

module Koshucode.Baala.Toolkit.Main.KoshuSyntax
( koshuSyntaxMain

-- * koshu-syntax.hs
-- $koshu-syntax.hs
) where

import Koshucode.Baala.Base.Syntax as Syn
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Toolkit.Library.Version
import Koshucode.Baala.Toolkit.Library.Exit
import Koshucode.Baala.Vanilla
import System.Console.GetOpt



-- ----------------------  Option

data Option
    = OptHelp
    | OptVersion
    | OptShowEncoding
    | OptRun
    | OptStdin
      deriving (Show, Eq, Ord, Enum, Bounded)

koshuOptions :: [OptDescr Option]
koshuOptions =
    [ Option "h" ["help"]     (NoArg OptHelp)    "Print help message."
    , Option "V" ["version"]  (NoArg OptVersion) "Print version number."
    , Option ""  ["run"]      (NoArg OptRun)     "Run section."
    , Option ""  ["show-encoding"] (NoArg OptShowEncoding) "Show character encoding."
    , Option "i" ["stdin"]    (NoArg OptStdin)  "Read from stdin."
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
          | otherwise           -> run files
          where has = (`elem` opts)
      (_, _, errs) -> putFailure $ concat errs

run :: [FilePath] -> IO ()
run paths = do
  mapM_ dumpTokenList paths

dumpTokenList :: FilePath -> IO ()
dumpTokenList path = 
    do code <- readFile path
       let ts = Syn.tokens code
           xs = zip [1 ..] ts
       putStr $ unlines h
       mapM_ putToken xs
    where
      h = [ "***"
          , "***  DESCRIPTION"
          , "***    Tokens from " ++ show path
          , "***"
          , "***  SUMMARY"
          , "***    Word     0"
          , "***    TermN    0"
          , "***    Open     0"
          , "***    Close    0"
          , "***    Space    0"
          , "***    Comment  0"
          , "***    Line     0"
          , "***"
          , "***  LEGEND"
          , "***    *** [line number] line content"
          , "***    |-- TOKEN /seq /type /token"
          , "***" ]

putToken :: (Int, Token) -> IO ()
putToken x@(_, Line (SourceLine n s)) =
    do putStrLn ""
       putStr "*** ["
       putStr $ show n
       putStr "] "
       putStrLn $ show s
       print $ doc $ tokenJudge x
putToken x = print $ doc $ tokenJudge x

tokenJudge :: (Int, Token) -> Judge Val
tokenJudge (n, t) = Judge True "TOKEN" args where
    args = [ ("/seq"   , intv n)
           , ("/type"  , stringv $ tokenType t)
           , ("/token" , stringv $ show t) ]

tokenType :: Token -> String
tokenType (Word _ _)  = "Word"
tokenType (TermN _)   = "TermN"
tokenType (TermP _)   = "TermP"
tokenType (Open _)    = "Open"
tokenType (Close _)   = "Close"
tokenType (Space _)   = "Space"
tokenType (Comment _) = "Comment"
tokenType (Line _)    = "Line"



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


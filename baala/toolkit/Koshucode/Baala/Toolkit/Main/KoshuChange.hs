{-# OPTIONS_GHC -Wall #-}

{-| Koshucode syntactic tool. -}

module Koshucode.Baala.Toolkit.Main.KoshuChange
( koshuChangeMain

-- * koshu-change.hs
-- $main

-- * Changeset
-- $Changeset
) where

import qualified Data.Set as S
import System.Console.GetOpt

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Section as Sec
import Koshucode.Baala.Vanilla

import Koshucode.Baala.Toolkit.Library.Exit
import Koshucode.Baala.Toolkit.Library.Version



-- ----------------------  Option

data Option
    = OptHelp
    | OptVersion
    | OptShowEncoding
    | OptRun
    | OptLeft
    | OptRight
    | OptMinus
    | OptFrom
    | OptTo
    | OptUpdate
      deriving (Show, Eq, Ord, Enum, Bounded)

koshuOptions :: [OptDescr Option]
koshuOptions =
    [ Option "h" ["help"]     (NoArg OptHelp)    "Print help message."
    , Option "V" ["version"]  (NoArg OptVersion) "Print version number."
    , Option ""  ["run"]      (NoArg OptRun)     "Run section."
    , Option ""  ["show-encoding"] (NoArg OptShowEncoding) "Show character encoding."
    , Option "l" ["left"]     (NoArg OptLeft)    "Standard input for left-hand side."
    , Option "r" ["right"]    (NoArg OptRight)   "Standard input for right-hand side."
    , Option "m" ["minus"]    (NoArg OptMinus)   "Output C.k = A.k - B.k"
    , Option "f" ["from"]     (NoArg OptFrom)    "Output C.k = A.k - B.k"
    , Option "t" ["to"]       (NoArg OptTo)      "Output C.k = B.k - A.k"
    , Option "u" ["update"]   (NoArg OptUpdate)  "Apply changeset C.k to B.k"
    ]

version :: String
version = "koshu-change-" ++ versionString

usage :: String
usage = usageInfo header koshuOptions

header :: String
header = unlines
    [ "DESCRIPTION"
    , "  Calculate changeset between two dataset."
    , ""
    , "USAGE"
    , "  koshu-change A.k -[mf] B.k"
    , "  koshu-change B.k -t A.k"
    , "  koshu-change B.k -u C.k"
    , ""
    ] ++ "OPTIONS"



-- ----------------------  Main

{-| The main function for @koshu-change@ command. -}
koshuChangeMain :: IO ()
koshuChangeMain = koshuChangeMain' =<< prelude

koshuChangeMain' :: (String, [String]) -> IO ()
koshuChangeMain' (_, argv) =
    case getOpt Permute koshuOptions argv of
      (opts, [p1, p2], [])
          -> run opts (File p1) (File p2)

      (opts, [p], [])
          | has OptLeft  -> run opts Stdin (File p)
          | has OptRight -> run opts (File p) Stdin
          where has = (`elem` opts)

      (opts, _, [])
          | has OptHelp         -> putSuccess usage
          | has OptVersion      -> putSuccess $ version ++ "\n"
          | has OptShowEncoding -> putSuccess =<< currentEncodings
          where has = (`elem` opts)

      (_, _, errs) -> putFailure $ concat errs ++ usage

    where
      run opts left right
          | has OptFrom    = left  `minus`  right
          | has OptTo      = right `minus`  left
          | has OptMinus   = left  `minus`  right
          | has OptUpdate  = left  `update` right
          | otherwise      = putFailure usage
          where has = (`elem` opts)



-- ----------------------  Input

data Input = Stdin | File FilePath
             deriving (Show, Eq)

inputText :: Input -> String
inputText (Stdin)  = "<stdin>"
inputText (File p) = p

readInput :: Input -> IO String
readInput (Stdin)  = getContents
readInput (File p) = readFile p

readInputs :: [Input] -> IO [String]
readInputs = mapM readInput

readJudge :: String -> [Judge Val]
readJudge s =
    let root = emptySection :: Section Val
    in case sectionRead root s of
         Right sec -> sectionJudge sec
         Left _    -> []



-- ----------------------  Output

putJudgeList :: (Ord v, Pretty v) => [Judge v] -> IO ()
putJudgeList = print . docv

putCommentLines :: [String] -> IO ()
putCommentLines = putStr . unlines . comment

comment :: Map [String]
comment xs = xs3 where
    xs3 = "** -*- koshu -*-" : xs2 ++ ["**", ""]
    xs2 = map ("**  " ++) $ "" : xs



-- ----------------------  Minus

minus :: Input -> Input -> IO ()
minus inputA inputB =
    do [textA, textB] <- readInputs [inputA, inputB]
       putCommentLines h
       putJudgeList $ readJudge textA `minusJudge` readJudge textB
    where
      h = [ "DATASETS"
          , "  There are changes C when altering dataset B into A."
          , ""
          , "  B (base)    : " ++ inputText inputA
          , "  A (altered) : " ++ inputText inputB
          , "  C (change)  : A - B"
          , ""
          , "UPDATE"
          , "  Dataset A is obtained by updating B by C."
          , "  Please execute: koshu-change B -u C"
          ]

minusJudge :: (Ord v) => [Judge v] -> [Judge v] -> [Judge v]
minusJudge judA judB = map denyJudge judgeC ++ judgeD where
    setA = S.fromList judA
    setB = S.fromList judB
    judgeC = S.toList $ setB `S.difference` setA
    judgeD = S.toList $ setA `S.difference` setB



-- ----------------------  Update

update :: Input -> Input -> IO ()
update inputB inputC =
    do [textB, textC] <- readInputs [inputB, inputC]
       putCommentLines h
       putJudgeList $ readJudge textB `updateJudge` readJudge textC
    where
      h = [ "DATASETS"
          , "  Updating dataset B by C, altered dataset A is obtained."
          , ""
          , "  B (base)    : " ++ inputText inputB
          , "  C (change)  : " ++ inputText inputC
          , "  A (altered) : B + C"
          ]

updateJudge :: (Ord v) => [Judge v] -> [Judge v] -> [Judge v]
updateJudge judB judC = judgeA where
    setB    = S.fromList $ judB
    denC    = S.fromList $ map affirmJudge $ filter isDenied judC
    affC    = S.fromList $ filter isAffirmed judC
    judgeA = S.toList $ setB `S.difference` denC `S.union` affC



-- ----------------------
-- $main
--
-- @koshu-change@ command is implemented using 'koshuChangeMain'.
--
-- > import Koshucode.Baala.Toolkit.Main.KoshuChange
-- > 
-- > main :: IO ()
-- > main = koshuChangeMain



-- ----------------------
{- $Changeset

Rules for calculating changeset C = A - B.
Altering dataset B into A, changeset C
is calculated by the following rules.

* Affirmed judge is in C if and only if the judge is not in A, and is in B.

* Denied judge is in C if and only if the judge is not in B, and is in A.

* Judge is not in C otherwise.

-}


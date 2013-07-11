{-# OPTIONS_GHC -Wall #-}

{-| Koshucode syntactic tool. -}

module Koshucode.Baala.Toolkit.Main.KoshuChange
( koshuChangeMain

-- * koshu-change.hs
-- $main
) where

import qualified Data.Set as S
import System.Console.GetOpt

import Koshucode.Baala.Base.Data

import Koshucode.Baala.Toolkit.Library.Exit
import Koshucode.Baala.Toolkit.Library.Version
import Koshucode.Baala.Toolkit.Library.Change



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
          | has OptFrom    = left  `minusInput` right
          | has OptTo      = right `minusInput` left
          | has OptMinus   = left  `minusInput` right
          | has OptUpdate  = left  `update`     right
          | otherwise      = putFailure usage
          where has = (`elem` opts)



-- ----------------------  Update

update :: Input -> Input -> IO ()
update inputB inputC =
    do [textB, textC] <- readInputs [inputB, inputC]
       putCommentLines h
       putJudges $ readJudge textB `updateJudge` readJudge textC
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


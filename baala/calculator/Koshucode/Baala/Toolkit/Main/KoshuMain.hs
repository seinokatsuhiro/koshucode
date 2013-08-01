{-# OPTIONS_GHC -Wall #-}

{-| A portable relational calculator. -}

module Koshucode.Baala.Toolkit.Main.KoshuMain
( koshuMain

-- * koshu.hs
-- $koshu.hs
) where

import System.Console.GetOpt

import Koshucode.Baala.Base hiding (text)
import Koshucode.Baala.Core
import Koshucode.Baala.Toolkit.Library.Exit
import Koshucode.Baala.Toolkit.Library.Run
import Koshucode.Baala.Toolkit.Library.Version
import qualified Koshucode.Baala.Base.Prelude.Pretty as Pretty


-- Flow
--
--  1. <stdin>         read from <stdin>
--  2. String          split into lines
--  3. [String]        tokenize each lines
--  4. [[Token]]       convert to judge
--  5. [Judge v]       gather into dataset
--  6. Dataset v       retrieve as relmap
--  7. Relmap v ...    edit using relmaps, convert relmap to judges
--  8. [[Judge v]]     concat list of judges
--  9. [Judge v]       convert to string representation
-- 10. [String]        concat lines
-- 11. String          write to <stdout>
-- 12. <stdout>



-- ----------------------  Option

data Option
    = OptHelp
    | OptVersion
    | OptShowEncoding
    | OptRun
    | OptPretty
    | OptStdin
    | OptSection String
    | OptCalc
    | OptListRop
      deriving (Show, Eq)

koshuOptions :: [OptDescr Option]
koshuOptions =
    [ Option "h" ["help"]     (NoArg OptHelp)    "Print help message."
    , Option "V" ["version"]  (NoArg OptVersion) "Print version number."
    , Option ""  ["run"]      (NoArg OptRun)     "Run section."
    , Option ""  ["show-encoding"] (NoArg OptShowEncoding) "Show character encoding."
    , Option ""  ["pretty"]   (NoArg OptPretty)  "Pretty print section."
    , Option "i" ["stdin"]    (NoArg OptStdin)   "Read from stdin."
    , Option "s" ["section"]  (ReqArg OptSection "SEC") "One-line section"
    , Option ""  ["calc"]     (NoArg OptCalc)    "Run calculation list"
    , Option ""  ["list-rop"] (NoArg OptListRop) "List relational operators"
    ]

version :: String
version = "koshu-" ++ versionString

usage :: String
usage = usageInfo header koshuOptions

header :: String
header = unlines
    [ "DESCRIPTION"
    , "  This is a relational data processor in Koshucode."
    , "  Koshucode is a notation for people and computers"
    , "  who read, write, and calculate relational data."
    , ""
    , "EXAMPLES"  -- TYPICAL USAGES
    , "  koshu CALC.k DATA1.k DATA2.k   Calculate using CALC.k"
    , "                                 for input DATAn.k"
    , "  koshu -i CALC.k < DATA.k       Read input data from"
    , "                                 standard input"
    , ""
    ] ++ "OPTIONS"



-- ----------------------  Main

{-| The main function for @koshu@ command.
    See 'Koshucode.Baala.Vanilla.Relmap.Implement.vanillaRops'
    for default argument. -}
koshuMain :: (CContent c) => [Rop c] -> IO ()
koshuMain rops =
  let cons = relmapCons rops
      root = makeEmptySection cons
  in koshuMain' rops root =<< prelude

koshuMain' :: (CContent c) => [Rop c] -> Section c -> (String, [String]) -> IO ()
koshuMain' rops root (_, argv) =
    case getOpt Permute koshuOptions argv of
      (opts, files, [])
          | has OptHelp         -> putSuccess usage
          | has OptVersion      -> putSuccess $ version ++ "\n"
          | has OptShowEncoding -> putSuccess =<< currentEncodings
          | has OptPretty       -> prettySection sec
          | has OptStdin        -> runStdin sec
          | has OptCalc         -> runCalc  sec
          | has OptListRop      -> putRop rops
          | otherwise           -> runFiles sec
          where has  = (`elem` opts)
                sec = SectionSource root text files
                text = concatMap oneLiner opts
      (_, _, errs) -> putFailure $ concat errs

putRop :: (Ord c, Pretty c, CText c) => [Rop c] -> IO ()
putRop rops = putJudges $ map f rops where
    f :: (CText c) => Rop c -> Judge c
    f Rop { ropName = n, ropGroup = g } =
        Judge True "KOSHU-ROP"
          [ ("/group" , putText g)
          , ("/name"  , putText n) ]

runStdin :: CContent c => SectionSource c -> IO ()
runStdin sec =
    do text <- getContents
       runFiles sec { textSections = text : textSections sec }

oneLiner :: Option -> [String]
oneLiner (OptSection sec) = [sec]
oneLiner _ = []




-- ----------------------  Pretty printing

prettySection :: (CContent c) => SectionSource c -> IO ()
prettySection (SectionSource root _ files) =
    case files of
      [file] -> do md <- sectionFile root file
                   prettyPrint md
      []     -> do s <- getContents
                   let md = sectionRead root s
                   prettyPrint md
      _      -> putSuccess usage
    where prettyPrint md = abortIO (print . Pretty.doc) md




-- -- -- -- -- -- -- -- -- --  

-- desc

-- desc :: (CContent v) => [Assert v] -> IO ()
-- desc = putStrLn . fromJudges . concatMap info where
--     info :: (CContent v) => Assert v -> [Judge v]
--     info a = input a -- ++ output a
--     affirm s arg = fmap putText $ Judge True s arg

--     input (Assert _ _ r) = concatMap input2 $ relmapSourceList r
--     input2 m = inputSign m : inputTerms m
--     inputSign (RelmapSource k _) = affirm "INPUT-SIGN" [("/sign", k)]
--     inputSign _ = undefined
--     inputTerms (RelmapSource k ns) = map (inputTerm k) ns
--     inputTerms _ = undefined
--     inputTerm k n = affirm "INPUT-TERM" [("/sign", k), ("/term", n)]

--     output (Assert _ s r) = output2 s r
--     output2 k m = outputSign k : outputTerms k m
--     outputSign k = affirm "OUTPUT-SIGN" [("/sign", k)]
--     outputTerms k m = outputHead k $ runRelmap emptyDataset m reldee
--     outputHead k (Rel h _) = map (outputTerm k) (headTerms h)
--     outputTerm k t = affirm "OUTPUT-TERM" [("/sign", k), ("/term", name t)]

-- fromJudges :: (CContent v) => [Judge v] -> String
-- fromJudges = unlines . unique . map string where
--     string = show . Pretty.doc

-- flow

-- flow :: [Assert v] -> IO ()
-- flow = putStrLn . show . Pretty.docv . map Pretty.doc

-- ----------------------
{- $koshu.hs
  
   @koshu@ command is implemented using 'koshuMain'.
  
   > import Koshucode.Baala.Toolkit.Main.KoshuMain
   > import Koshucode.Baala.Vanilla
   > 
   > main :: IO ()
   > main = koshuMain vanillaRops
-}


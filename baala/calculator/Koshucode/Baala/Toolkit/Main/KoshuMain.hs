{-# OPTIONS_GHC -Wall #-}

{-| A portable relational calculator. -}

module Koshucode.Baala.Toolkit.Main.KoshuMain
( koshuMain

-- * koshu.hs
-- $koshu.hs
) where

import System.Console.GetOpt
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Toolkit.Library.Element  as L
import qualified Koshucode.Baala.Toolkit.Library.Exit     as L
import qualified Koshucode.Baala.Toolkit.Library.Run      as L
import qualified Koshucode.Baala.Toolkit.Library.Version  as L


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
    | OptElement
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
    , Option ""  ["element"]  (NoArg OptElement) "Analize sections"
    ]

version :: String
version = "koshu-" ++ L.versionString

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
koshuMain :: (C.CContent c) => [C.Rop c] -> IO ()
koshuMain rops =
  let cons = C.relmapCons rops
      root = C.makeEmptySection cons
  in koshuMain' rops root =<< L.prelude

koshuMain' :: (C.CContent c) => [C.Rop c] -> C.Section c -> (String, [String]) -> IO ()
koshuMain' rops root (_, argv) =
    case getOpt Permute koshuOptions argv of
      (opts, files, [])
          | has OptHelp         -> L.putSuccess usage
          | has OptVersion      -> L.putSuccess $ version ++ "\n"
          | has OptShowEncoding -> L.putSuccess =<< L.currentEncodings
          | has OptPretty       -> prettySection sec
          | has OptStdin        -> runStdin sec
          | has OptCalc         -> L.runCalc  sec
          | has OptListRop      -> putRop   rops
          | has OptElement      -> putElems sec
          | otherwise           -> L.runFiles sec
          where has  = (`elem` opts)
                sec  = L.SectionSource root text files
                text = concatMap oneLiner opts
      (_, _, errs) -> L.putFailure $ concat errs

putRop :: (Ord c, B.Pretty c, C.CText c) => [C.Rop c] -> IO ()
putRop rops = B.putJudges $ map f rops where
    f :: (C.CText c) => C.Rop c -> B.Judge c
    f C.Rop { C.ropName = n, C.ropGroup = g } =
        B.Judge True "KOSHU-ROP"
          [ ("/group" , C.putText g)
          , ("/name"  , C.putText n) ]

runStdin :: (C.CContent c) => L.SectionSource c -> IO ()
runStdin sec =
    do text <- getContents
       L.runFiles sec { L.textSections = text : L.textSections sec }

oneLiner :: Option -> [String]
oneLiner (OptSection sec) = [sec]
oneLiner _ = []

putElems :: (C.CContent c) => L.SectionSource c -> IO ()
putElems src =
    do ass <- L.readSecList src
       case ass of
         Right ss -> B.putJudges $ concatMap L.sectionElem ss
         Left  _  -> B.bug


-- ----------------------  Pretty printing

prettySection :: (C.CContent c) => L.SectionSource c -> IO ()
prettySection (L.SectionSource root _ files) =
    case files of
      [file] -> do md <- C.sectionFile root file
                   prettyPrint md
      []     -> do s <- getContents
                   let md = C.sectionRead root "" s
                   prettyPrint md
      _      -> L.putSuccess usage
    where prettyPrint md = B.abortIO (print . B.doc) md




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
--     string = show . B.doc

-- flow

-- flow :: [Assert v] -> IO ()
-- flow = putStrLn . show . B.docv . map B.doc

-- ----------------------
{- $koshu.hs
  
   @koshu@ command is implemented using 'koshuMain'.
  
   > import Koshucode.Baala.Toolkit.Main.KoshuMain
   > import Koshucode.Baala.Vanilla
   > 
   > main :: IO ()
   > main = koshuMain vanillaRops
-}


{-# OPTIONS_GHC -Wall #-}

{-| A portable relational calculator. -}

module Koshucode.Baala.Toolkit.Main.KoshuMain
( koshuMain

-- * koshu.hs
-- $koshu.hs
) where

import Data.Monoid
import System.Console.GetOpt
import System.IO

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Toolkit.Library.Comment
import Koshucode.Baala.Toolkit.Library.Exit
import Koshucode.Baala.Toolkit.Library.Version
import qualified Koshucode.Baala.Base.Prelude.Pretty as Pretty
import qualified Koshucode.Baala.Base.Prelude as Kit
import qualified Koshucode.Baala.Base.Section as Kit
import qualified Koshucode.Baala.Base.Data    as Kit
import qualified Koshucode.Baala.Minimal.OpKit as Kit


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
    See 'Koshucode.Baala.Vanilla.Relmap.Implement.vanillaOperators'
    for default argument. -}
koshuMain :: (Value v) => [Kit.OpImplement v] -> IO ()
koshuMain relmaps =
  let cons = Kit.relmapCons relmaps
      root = Kit.makeEmptySection cons
  in koshuMain' root =<< prelude

koshuMain' :: (Value v) => Kit.Section v -> (String, [String]) -> IO ()
koshuMain' root (_, argv) =
    case getOpt Permute koshuOptions argv of
      (opts, files, [])
          | has OptHelp         -> putSuccess usage
          | has OptVersion      -> putSuccess $ version ++ "\n"
          | has OptShowEncoding -> putSuccess =<< currentEncodings
          | has OptPretty       -> prettySection root files
          | has OptStdin        -> runStdin opts root files
          | has OptCalc         -> runCalc  opts root files
          | otherwise           -> runFiles opts root files
          where has = (`elem` opts)
      (_, _, errs) -> putFailure $ concat errs



-- ----------------------  Run

{-| Read and union sections from files, and run the section. -}
runFiles :: (Value v) => [Option] -> Kit.Section v -> [FilePath] -> IO ()
runFiles = hRunFiles stdout

hRunFiles :: (Value v) => Handle -> [Option] -> Kit.Section v -> [FilePath] -> IO ()
hRunFiles h opts root files =
    do let sec = concatMap (oneLiner root) opts
       sects <- mapM (Kit.sectionFile root) files
       let union = concatMM $ sec ++ sects
           comm  = CommentDoc
                   [ CommentSec "INPUT" files]
       hSetEncoding h utf8
       hPutStrLn h emacsModeComment
       hPutStr h $ unlines $ texts comm
       hPutStrLn h ""
       abortIO (Kit.hRunSectionIO h) union

runStdin :: (Value v) => [Option] -> Kit.Section v -> [FilePath] -> IO ()
runStdin opts root files = do
  input <- getContents
  let sec1 = Kit.sectionRead root input
      sec2 = concatMap (oneLiner root) opts
  sects <- mapM (Kit.sectionFile root) files
  abortIO Kit.runSectionIO $ concatMM (sec1 : sec2 ++ sects)

oneLiner :: (Value v) => Kit.Section v -> Option -> [(AbortOr (Kit.Section v))]
oneLiner root (OptSection sec) = [Kit.sectionRead root sec]
oneLiner _ _ = []

concatMM :: (Monad m, Monoid a) => [m a] -> m a
concatMM [] = return mempty
concatMM (s:ss) = do s'  <- s
                     ss' <- concatMM ss
                     return $ mappend s' ss'



-- ---------------------- Calculation list

runCalc :: (Value v) => [Option] -> Kit.Section v -> [FilePath] -> IO ()
runCalc opts root files =
    do let sec = concatMap (oneLiner root) opts
       sects <- mapM (Kit.sectionFile root) files
       let union = concatMM $ sec ++ sects
       abortIO (runCalcSec opts root) union

runCalcSec :: (Value v) => [Option] -> Kit.Section v -> Kit.Section v -> IO ()
runCalcSec opts root sec =
    do let js = Kit.sectionJudge sec
       mapM_ (runCalcJudge opts root) js
       return ()

runCalcJudge :: (Value v) => [Option] -> Kit.Section v -> Judge v -> IO ()
runCalcJudge opts root (Judge True "KOSHU-CALC" xs) =
    case theContents ["/input", "/output"] xs of
      Just [input, output] ->
          do let inputFiles = theStrings input
                 outputFile = Kit.theStringValue output
             putStrLn $ "**  Output to " ++ outputFile
             withFile outputFile WriteMode
                          $ \ h -> hRunFiles h opts root inputFiles
      Just _       -> return ()
      Nothing      -> return ()
runCalcJudge _ _ _ =  return ()



-- ----------------------  Pretty printing

prettySection :: (Value v) => Kit.Section v -> [FilePath] -> IO ()
prettySection root files =
    case files of
      [file] -> do md <- Kit.sectionFile root file
                   prettyPrint md
      []     -> do s <- getContents
                   let md = Kit.sectionRead root s
                   prettyPrint md
      _      -> putSuccess usage
    where prettyPrint md = abortIO (print . Pretty.doc) md



-- ----------------------  The

theContent :: (Value c) => String -> [Kit.Named c] -> Maybe c
theContent = lookup

theContents :: (Value c) => [String] -> [Kit.Named c] -> Maybe [c]
theContents ns termset = mapM (`theContent` termset) ns

theStrings :: (Value c) => c -> [String]
theStrings c | Kit.isStringValue c = [Kit.theStringValue c]
theStrings c | Kit.isListValue c   = map Kit.theStringValue
                                     $ Kit.theListValue c
theStrings _ = []



-- -- -- -- -- -- -- -- -- --  

-- desc

-- desc :: (Value v) => [Kit.Assert v] -> IO ()
-- desc = putStrLn . fromJudges . concatMap info where
--     info :: (Value v) => Kit.Assert v -> [Judge v]
--     info a = input a -- ++ output a
--     affirm s arg = fmap stringValue $ Judge True s arg

--     input (Kit.Assert _ _ r) = concatMap input2 $ Kit.relmapSourceList r
--     input2 m = inputSign m : inputTerms m
--     inputSign (Kit.RelmapSource k _) = affirm "INPUT-SIGN" [("/sign", k)]
--     inputSign _ = undefined
--     inputTerms (Kit.RelmapSource k ns) = map (inputTerm k) ns
--     inputTerms _ = undefined
--     inputTerm k n = affirm "INPUT-TERM" [("/sign", k), ("/term", n)]

--     output (Kit.Assert _ s r) = output2 s r
--     output2 k m = outputSign k : outputTerms k m
--     outputSign k = affirm "OUTPUT-SIGN" [("/sign", k)]
--     outputTerms k m = outputHead k $ Kit.runRelmap emptyDataset m reldee
--     outputHead k (Rel h _) = map (outputTerm k) (headTerms h)
--     outputTerm k t = affirm "OUTPUT-TERM" [("/sign", k), ("/term", name t)]

-- fromJudges :: (Value v) => [Judge v] -> String
-- fromJudges = unlines . unique . map string where
--     string = show . Pretty.doc

-- flow

-- flow :: [Kit.Assert v] -> IO ()
-- flow = putStrLn . show . Pretty.docv . map Pretty.doc

-- ----------------------
-- $koshu.hs
--
-- @koshu@ command is implemented using 'koshuMain'.
--
-- @
-- import Koshucode.Baala.Toolkit.Main.KoshuMain
-- import Koshucode.Baala.Vanilla
-- @
--
-- @
-- main :: IO ()
-- main = koshuMain vanillaOperators
-- @


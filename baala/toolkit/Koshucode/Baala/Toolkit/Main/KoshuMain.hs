{-# OPTIONS_GHC -Wall #-}

-- | Koshucode relational data processor.

module Koshucode.Baala.Toolkit.Main.KoshuMain
( koshuMain
) where
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Toolkit.Library.Version
import Koshucode.Baala.Toolkit.Library.Exit
import System.Console.GetOpt
import qualified Koshucode.Baala.Base.Struct as Struct
import qualified Koshucode.Baala.Base.Kit as Kit
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
      deriving (Show, Eq, Ord, Enum, Bounded)

koshuOptions :: [OptDescr Option]
koshuOptions =
    [ Option "h" ["help"]     (NoArg OptHelp)    "Print help message."
    , Option "V" ["version"]  (NoArg OptVersion) "Print version number."
    , Option ""  ["run"]      (NoArg OptRun)     "Run section."
    , Option ""  ["show-encoding"] (NoArg OptShowEncoding) "Show character encoding."
    , Option ""  ["pretty"]   (NoArg OptPretty) "Pretty print section."
    ]

version :: String
version = "koshu-" ++ versionString

usage :: String -> String
usage prog = usageInfo (header prog) koshuOptions

header :: String -> String
header prog = unlines
    [ "USAGE: " ++ prog ++ " [OPTION ...] CALC.k < INPUT.k > OUTPUT.k"
    , "  A relational data processor in koshucode."
    , ""
    ] ++ "OPTIONS:"



-- ----------------------  Main

-- | The main function for @koshu@ command.
--   See 'Koshucode.Baala.Vanilla.Relmap.vanillaRelmaps' for default argument.
koshuMain :: (Value v) => [Struct.RelmapImplement v] -> IO ()
koshuMain relmaps =
  let cons = Struct.makeConsRelmap relmaps
      root = Kit.makeEmptySection cons
  in koshuMain' root =<< prelude

koshuMain' :: (Value v) => Kit.Section v -> (String, [String]) -> IO ()
koshuMain' root (prog, argv) =
    case getOpt Permute koshuOptions argv of
      (opts, files, [])
          | has OptHelp         -> putSuccess $ usage prog
          | has OptVersion      -> putSuccess version
          | has OptShowEncoding -> putSuccess =<< currentEncodings
          | has OptPretty       -> prettySection root prog files
          | otherwise           -> runSection root prog files
          where has = (`elem` opts)
      (_, _, errs) -> putFailure $ concat errs

runSection :: (Value v) => Kit.Section v -> String -> [FilePath] -> IO ()
runSection root _ [file] = do
  calc <- Kit.sectionFile root file
  abortIO Kit.runSectionIO calc
runSection _ prog _ = putSuccess $ usage prog

prettySection :: (Value v) => Kit.Section v -> String -> [FilePath] -> IO ()
prettySection root prog files =
    case files of
      [file] -> do md <- Kit.sectionFile root file
                   prettyPrint md
      []     -> do stdin <- getContents
                   let md = Kit.sectionRead root stdin
                   prettyPrint md
      _      -> putSuccess $ usage prog
    where prettyPrint md = abortIO (print . Pretty.doc) md



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


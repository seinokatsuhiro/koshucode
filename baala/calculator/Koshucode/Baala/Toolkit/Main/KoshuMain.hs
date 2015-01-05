{-# OPTIONS_GHC -Wall #-}

-- | A portable relational calculator.

module Koshucode.Baala.Toolkit.Main.KoshuMain
  ( koshuMain
  
  -- * koshu.hs
  -- $koshu.hs
  ) where

import System.Console.GetOpt
import qualified Data.Time as T
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
--  5. [Judge c]       gather into dataset
--  6. Dataset c       retrieve as relmap
--  7. [Relmap c]      edit using relmaps, convert relmap to judges
--  8. [[Judge c]]     concat list of judges
--  9. [Judge c]       convert to string representation
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

-- | The main function for @koshu@ command.
--   See 'Koshucode.Baala.Op.Vanilla.Relmap.Implement.vanillaRops'
--   for default argument.
koshuMain :: (C.CContent c) => C.Global c -> IO Int
koshuMain global =
  do (prog, argv) <- L.prelude
     time <- T.getZonedTime
     let day = T.localDay $ T.zonedTimeToLocalTime time
     case getOpt Permute koshuOptions argv of
       (opts, files, [])
           | has OptHelp         ->  L.putSuccess usage
           | has OptVersion      ->  L.putSuccess $ version ++ "\n"
           | has OptShowEncoding ->  L.putSuccess =<< L.currentEncodings
           -- has OptPretty    ->  prettySection bun
           | has OptElement      ->  putElems   bun
           | has OptCalc         ->  L.runCalc  cmd bun
           | otherwise           ->  L.runFiles g2 bun
           where
             has   =  (`elem` opts)
             bun   =  C.SourceBundle root res
             text  =  concatMap oneLiner opts
             cmd   =  prog : argv
             root  =  C.resEmpty { C.resGlobal = g2 }
             res   =  B.sourceList (has OptStdin) text files []
             g2    =  C.globalFill global
                        { C.globalProgram   = prog
                        , C.globalArgs      = argv
                        , C.globalTime      = B.timeYmd day
                        , C.globalSources   = res }

       (_, _, errs) -> L.putFailure $ concat errs

oneLiner :: Option -> [String]
oneLiner (OptSection sec) = [oneLinerPreprocess sec]
oneLiner _ = []

-- replace "||" to "\n"
oneLinerPreprocess :: B.Map String
oneLinerPreprocess = loop where
    loop [] = []
    loop ('|' : '|' : xs) = '\n' : loop (B.trimLeft xs)
    loop (x : xs) = x : loop xs

putElems :: (C.CContent c) => C.SourceBundle c -> IO Int
putElems bun =
    do ass <- L.readSecList bun
       case ass of
         Right ss -> B.putJudges 0 $ concatMap L.resourceElem ss
         Left  _  -> B.bug "putElems"


-- ----------------------  Pretty printing

-- prettySection :: (C.CContent c) => C.SourceBundle c -> IO Int
-- prettySection (C.SourceBundle root _ files _) =
--     case files of
--       [file] -> do md <- C.readSection root (B.SourceFile file)
--                    prettyPrint md
--                    return 0
--       []     -> do text <- getContents
--                    md <- C.readSection root (B.SourceText text)
--                    prettyPrint md
--                    return 0
--       _      -> L.putSuccess usage
--     where prettyPrint md' =
--               case md' of
--                 Left a   -> B.abort [] a
--                 Right md -> print $ B.doc md




-- -- -- -- -- -- -- -- -- --  

-- desc

-- desc :: (CContent v) => [Assert v] -> IO ()
-- desc = putStrLn . fromJudges . concatMap info where
--     info :: (CContent v) => Assert v -> [Judge v]
--     info a = input a -- ++ output a
--     affirm s arg = fmap pText $ Judge True s arg

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
--     outputTerms k m = outputHead k $ runRelmap datasetEmpty m reldee
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
   > import Koshucode.Baala.Op.Vanilla
   > 
   > main :: IO ()
   > main = koshuMain vanillaRops
-}


{-# OPTIONS_GHC -Wall #-}

-- | A portable relational calculator.

module Koshucode.Baala.Toolkit.Main.KoshuMain
  ( koshuMain
  
  -- * koshu.hs
  -- $koshu.hs
  ) where

import qualified Data.Time              as T
import qualified Koshucode.Baala.Base   as B
import qualified Koshucode.Baala.Data   as D
import qualified Koshucode.Baala.Core   as C
import qualified Koshucode.Baala.Writer as W
import qualified Koshucode.Baala.Toolkit.Library.Element       as L
import qualified Koshucode.Baala.Toolkit.Library.Exit          as L
import qualified Koshucode.Baala.Toolkit.Library.Run           as L
import qualified Koshucode.Baala.Toolkit.Library.SimpleOption  as Opt


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



-- ----------------------  Parameter

data Param c = Param
    { paramElement       :: Bool
    , paramWriter        :: C.ResultWriter c
    , paramLiner         :: [B.Bz]
    , paramPretty        :: Bool
    , paramRun           :: Bool
    , paramShowEncoding  :: Bool
    , paramStdin         :: Bool
    , paramHelp          :: Bool
    , paramVersion       :: Bool

    , paramProg          :: String
    , paramArgs          :: [String]
    , paramProxy         :: [(String, Maybe String)]
    , paramTime          :: T.ZonedTime
    , paramDay           :: T.Day
    } deriving (Show)

initParam :: (Show c, D.CContent c, W.ToJSON c) => Opt.ParseResult -> IO (Param c)
initParam (Left errs) = L.putFailure $ concat errs
initParam (Right (opts, args)) =
    do (prog, _) <- L.prelude
       proxy  <- L.getProxies
       time   <- T.getZonedTime
       let day = T.localDay $ T.zonedTimeToLocalTime time
       return $ Param { paramElement       = getFlag "element"
                      , paramWriter        = writer
                      , paramLiner         = map B.stringBz liner
                      , paramPretty        = getFlag "pretty"
                      , paramRun           = getFlag "run"
                      , paramShowEncoding  = getFlag "show-encoding"
                      , paramStdin         = getFlag "stdin"
                      , paramHelp          = getFlag "help"
                      , paramVersion       = getFlag "version"

                      , paramProg          = prog
                      , paramArgs          = args
                      , paramProxy         = proxy
                      , paramTime          = time
                      , paramDay           = day }
    where
      getFlag  = Opt.getFlag opts
      getReq   = Opt.getReq  opts

      assertX  = getReq "assert-x"

      liner  | null assertX = map oneLiner $ getReq "liner"
             | otherwise    = ["|== X : add /x ( " ++ concat assertX ++ " )"]

      writer | getFlag "csv"            = W.resultCsv
             | getFlag "csv-heading"    = W.resultCsvHeading
             | getFlag "tsv-heading"    = W.resultTsvHeading
             | getFlag "dump"           = C.resultDump
             | getFlag "geojson"        = W.resultGeoJson
             | getFlag "html-compact"   = W.resultHtmlCompact
             | getFlag "html-indented"  = W.resultHtmlIndented
             | getFlag "json"           = W.resultJson
             | otherwise                = W.resultKoshu

      -- replace "||" to "\n"
      oneLiner :: B.Map String
      oneLiner []               = []
      oneLiner ('|' : '|' : xs) = '\n' : oneLiner (B.trimLeft xs)
      oneLiner (x : xs)         = x : oneLiner xs


-- ----------------------  Main

help :: [String]
help =
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
    ]

options :: Opt.SimpleOptions
options =
    [ Opt.help
    , Opt.version
    , Opt.flag "i" ["stdin"]                   "Read from stdin"
    , Opt.req  "x" ["assert-x"] "EXPR"         "|== X : add /x ( EXPR )"
    , Opt.flag ""  ["element"]                 "Analize input code"
    , Opt.flag ""  ["html-indented", "html"]   "HTML output with indent"
    , Opt.flag ""  ["html-compact"]            "HTML output without indent"
    , Opt.flag ""  ["csv"]                     "CSV output with judgement"
    , Opt.flag ""  ["csv-heading"]             "CSV output with heading"
    , Opt.flag ""  ["tsv-heading"]             "TSV output with heading"
    , Opt.flag ""  ["dump"]                    "Dump internal data"
    , Opt.flag ""  ["json"]                    "JSON output"
    , Opt.flag ""  ["geojson"]                 "GeoJSON output"
    , Opt.req  ""  ["liner"] "CODE"            "One liner"
    , Opt.flag ""  ["pretty"]                  "Pretty print"
    , Opt.flag ""  ["run"]                     "Run input code"
    , Opt.flag ""  ["show-encoding"]           "Show character encoding"
    ]

-- | The main function for @koshu@ command.
--   See 'Koshucode.Baala.Op.Global.baalaRops'
--   for default argument.
koshuMain :: (D.CContent c, W.ToJSON c) => C.Global c -> IO B.ExitCode
koshuMain g = Opt.parseCommand options >>= initParam >>= koshuMainParam g

koshuMainParam :: (D.CContent c, W.ToJSON c) => C.Global c -> Param c -> IO B.ExitCode
koshuMainParam g p
    | paramHelp p          = L.putSuccess $ Opt.helpMessage help options
    | paramVersion p       = L.putSuccess $ ver ++ "\n"
    | paramShowEncoding p  = L.putSuccess =<< L.currentEncodings
    | paramElement p       = putElems   g2 src
    | otherwise            = L.runFiles g2 src
    where
      ver   = C.globalSynopsis g ++ " " ++ C.globalVersionText g
      src   = B.ioPointList (paramStdin p) (paramLiner p) "" (paramArgs p)

      -- global parameter
      rslt  = (C.globalResult g) { C.resultWriter = paramWriter p }
      root  = C.resEmpty { C.resGlobal = g2 }
      g2    = C.globalFill g
              { C.globalProgram   = paramProg p
              , C.globalArgs      = paramArgs p
              , C.globalProxy     = paramProxy p
              , C.globalTime      = D.timeYmd $ paramDay p
              , C.globalResult    = rslt
              , C.globalHook      = root }

putElems :: (D.CContent c) => C.Global c -> [B.IOPoint] -> IO B.ExitCode
putElems g src =
    do (abres, _) <- C.gioResource (C.readSources src) g
       res2 <- abio abres
       res3 <- abio $ C.assembleRelmap res2
       putStrLn "-*- koshu -*-"
       putStrLn ""
       W.putJudgesWith (B.exitCode 0) $ L.resourceElem res3
    where
      abio mx = case mx of
                  Left  a -> B.abort [] a
                  Right x -> return x


-- ----------------------  Pretty printing

-- prettySection :: (D.CContent c) => C.SourceBundle c -> IO Int
-- prettySection (C.SourceBundle root _ files _) =
--     case files of
--       [file] -> do md <- C.readSection root (B.CodeFile file)
--                    prettyPrint md
--                    return 0
--       []     -> do text <- getContents
--                    md <- C.readSection root (B.CodeText text)
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
  
   > import qualified Koshucode.Baala.Base                     as B
   > import qualified Koshucode.Baala.Core                     as C
   > import qualified Koshucode.Baala.Type.Vanilla             as Type
   > import qualified Koshucode.Baala.Op.Global                as Op
   > import qualified Koshucode.Baala.Toolkit.Main.KoshuMain   as Main
   > import qualified Koshucode.Baala.Toolkit.Library.Version  as Main
   > 
   > main :: IO ()
   > main = B.exitWith =<< Main.koshuMain koshuGlobal
   > 
   > koshuGlobal :: Type.GlobalC
   > koshuGlobal = Op.baalaGlobal
   >               { C.globalSynopsis  = "The Koshucode Baala Implementation"
   >               , C.globalVersion   = Main.version }
-}


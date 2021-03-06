{-# OPTIONS_GHC -Wall #-}

-- | A portable relational calculator.

module Koshucode.Baala.Toolkit.Main.KoshuMain
  ( koshuMain
    -- * koshu.hs
    -- $koshu.hs
  ) where

import qualified Koshucode.Baala.Overture                 as O
import qualified Koshucode.Baala.System                   as O
import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Type                     as T
import qualified Koshucode.Baala.Data                     as D
import qualified Koshucode.Baala.Core                     as C
import qualified Koshucode.Baala.Writer                   as W
import qualified Koshucode.Baala.System.CliParser         as Z
import qualified Koshucode.Baala.Toolkit.Library.Element  as L
import qualified Koshucode.Baala.Toolkit.Library.Run      as L

{- $koshu.hs
  
   @koshu@ command is implemented using 'koshuMain'.
  
   > import qualified Koshucode.Baala.Base                     as B
   > import qualified Koshucode.Baala.Core                     as C
   > import qualified Koshucode.Baala.Toolkit.Library.Global   as Gl
   > import qualified Koshucode.Baala.Toolkit.Library.Version  as Ver
   > import qualified Koshucode.Baala.Toolkit.Main.KoshuMain   as Main
   > 
   > main :: IO ()
   > main = B.exitWith =<< Main.koshuMain koshuGlobal
   > 
   > koshuGlobal :: C.GlobalC
   > koshuGlobal = Gl.baalaGlobal
   >               { C.globalSynopsis  = "The Koshucode Baala Implementation"
   >               , C.globalVersion   = Ver.version }
-}

-- ----------------------  Parameter

data Param c = Param
    { paramAutoOutput    :: Bool
    , paramElement       :: Bool
    , paramWriter        :: C.ResultWriter c
    , paramLiner         :: [O.Bz]
    , paramPretty        :: Bool
    , paramRun           :: Bool
    , paramShowEncoding  :: Bool
    , paramStdin         :: Bool
    , paramOutput        :: Maybe FilePath
    , paramHelp          :: Bool
    , paramVersion       :: Bool

    , paramProg          :: String
    , paramArgs          :: [String]
    , paramProxy         :: [(String, Maybe String)]
    , paramNow           :: T.Time
    } deriving (Show)

initParam :: (Show c, D.CContent c, W.ToJSON c) => Z.Parsed -> IO (Param c)
initParam (Left errs) = O.putFailure $ concat errs
initParam (Right (z, args)) =
    do (prog, _) <- B.progAndArgs
       proxy     <- L.getProxies
       now       <- currentTime
       return $ Param { paramAutoOutput    = getFlag "auto-output"
                      , paramElement       = getFlag "element"
                      , paramWriter        = writer
                      , paramLiner         = map B.stringBz liner
                      , paramPretty        = getFlag "pretty"
                      , paramRun           = getFlag "run"
                      , paramShowEncoding  = getFlag "show-encoding"
                      , paramStdin         = getFlag "stdin"
                      , paramOutput        = getLast "output"
                      , paramHelp          = getFlag "help"
                      , paramVersion       = getFlag "version"

                      , paramProg          = prog
                      , paramArgs          = args
                      , paramProxy         = proxy
                      , paramNow           = now }
    where
      getFlag  = Z.getFlag z
      getReq   = Z.getReq  z
      getLast  = Z.getReqLast z

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
             | getFlag "tab"            = W.resultKoshuTab
             | otherwise                = W.resultKoshu

      -- replace "||" to "\n"
      oneLiner :: O.StringMap
      oneLiner []               = []
      oneLiner ('|' : '|' : xs) = '\n' : oneLiner (O.trimBegin xs)
      oneLiner (x : xs)         = x : oneLiner xs

      currentTime = case D.stringTime $ unwords $ getReq "now" of
                      Right t -> return t
                      Left _  -> T.nowZoned


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

options :: [Z.Option]
options =
    [ Z.help
    , Z.version
    , Z.req  "x" ["assert-x"] "EXPR"         "|== X : add /x ( EXPR )"
    , Z.flag ""  ["auto-output"]             "Automatic output when no assertions"
    , Z.flag ""  ["csv"]                     "CSV output with judgement"
    , Z.flag ""  ["csv-heading"]             "CSV output with heading"
    , Z.flag ""  ["dump"]                    "Dump internal data"
    , Z.flag ""  ["element"]                 "Analize input code"
    , Z.flag ""  ["geojson"]                 "GeoJSON output"
    , Z.flag ""  ["html-indented", "html"]   "HTML output with indent"
    , Z.flag ""  ["html-compact"]            "HTML output without indent"
    , Z.flag ""  ["json"]                    "JSON output"
    , Z.req  ""  ["liner"] "CODE"            "One liner"
    , Z.req  ""  ["now"] "CODE"              "Set system time"
    , Z.req  "o" ["output"] "FILE.k"         "Output file"
    , Z.flag ""  ["pretty"]                  "Pretty print"
    , Z.flag ""  ["run"]                     "Run input code"
    , Z.flag ""  ["show-encoding"]           "Show character encoding"
    , Z.flag "i" ["stdin"]                   "Read from stdin"
    , Z.flag ""  ["tab"]                     "Tab-separated output"
    , Z.flag ""  ["tsv-heading"]             "TSV output with heading"
    ]

-- | The main function for @koshu@ command.
--   See 'Koshucode.Baala.Toolkit.Library.Global.baalaRops'
--   for default argument.
koshuMain :: (D.CContent c, W.ToJSON c) => C.Global c -> IO B.ExitCode
koshuMain g = Z.parseCommand options >>= initParam >>= koshuMainParam g

koshuMainParam :: (D.CContent c, W.ToJSON c) => C.Global c -> Param c -> IO B.ExitCode
koshuMainParam g@C.Global { C.globalResult  = rslt
                          , C.globalFeature = feat
                          , C.globalHook    = res }
               p
    | paramHelp p          = O.putSuccess $ Z.helpMessage help options
    | paramVersion p       = O.putSuccessLn ver
    | paramShowEncoding p  = O.putSuccessLn =<< B.currentEncodings
    | paramElement p       = putElems   g2 src
    | otherwise            = L.runFiles g2 src
    where
      ver   = C.globalSynopsis g ++ " " ++ C.globalVersionText g
      src   = B.ioPointTogether (paramStdin p) (paramLiner p) (paramArgs p)

      -- global parameter
      g2 = C.globalFill g
           { C.globalFeature   = feat { C.featAutoOutput = paramAutoOutput p }
           , C.globalResult    = rslt { C.resultWriter = paramWriter p }
           , C.globalProgram   = paramProg  p
           , C.globalArgs      = paramArgs  p
           , C.globalProxy     = paramProxy p
           , C.globalTime      = paramNow   p
           , C.globalHook      = altMaybe resOutputAlt (paramOutput p) res }

-- | Alter output of data resource.
resOutputAlt :: FilePath -> O.Map (C.Resource c)
resOutputAlt path res = res { C.resOutput = B.ioPoint path }

-- | Alter only if 'Just' value is given.
altMaybe :: (a -> b -> b)         -- ^ Pure alteration
         -> (Maybe a -> b -> b)   -- ^ Maybe alteration
altMaybe _ (Nothing) b  = b
altMaybe f (Just a)  b  = f a b

putElems :: (D.CContent c) => C.Global c -> [B.IOPoint] -> IO B.ExitCode
putElems g ns =
    do (abres, _) <- C.resReadPoints g ns
       res2 <- B.abortLeft abres
       res3 <- B.abortLeft $ C.assembleRelmap res2
       putStrLn "-*- koshu -*-"
       putStrLn ""
       W.putJudges $ L.resourceElem res3
       return $ O.exitCode 0


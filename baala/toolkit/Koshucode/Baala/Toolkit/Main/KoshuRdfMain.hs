{-# OPTIONS_GHC -Wall #-}

-- | RDF-to-Koshucode converter.

module Koshucode.Baala.Toolkit.Main.KoshuRdfMain
( koshuRdfMain
) where
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Toolkit.Library.Exit
import Koshucode.Baala.Toolkit.Library.RDF
import Koshucode.Baala.Toolkit.Library.Version
import Koshucode.Baala.Vanilla
import System.Console.GetOpt
import qualified Data.List                as List
import qualified Data.RDF                 as RDF
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import qualified Text.RDF.RDF4H.XmlParser as RDF



-- ----------------------  Main

koshuRdfMain :: IO ()
koshuRdfMain = do
  (opts, files) <- parseCommand =<< prelude
  checkOptions opts
  mapM_ (convert opts) files

data Option
    = OptHelp
    | OptVersion
    | OptTurtle
    | OptXml
    | Opt2
    | Opt3
      deriving (Show, Eq, Ord, Enum, Bounded)

options :: [OptDescr Option]
options =
    [ Option "h" ["help"]     (NoArg OptHelp)    "Print help message."
    , Option "V" ["version"]  (NoArg OptVersion) "Print version number."
    , Option "t" ["turtle"]   (NoArg OptTurtle)  "RDF format is Turtle."
    , Option "x" ["xml",
                  "rdfxml"]   (NoArg OptXml)     "RDF format is RDF/XML."
    , Option "2" ["binary"]   (NoArg Opt2)       "Binary export (/s /o)"
    , Option "3" ["ternary"]  (NoArg Opt3)       "Ternary export (/s /p /o)"
    ]

checkOptions :: [Option] -> IO ()
checkOptions opts | more opts [OptTurtle, OptXml] =
  putFailure "choose one of -n -t -x"
checkOptions opts | more opts [Opt2, Opt3] =
  putFailure "choose one of -2 -3"
checkOptions _ = return ()

more :: (Eq a) => [a] -> [a] -> Bool
more opts = isMultiple . List.intersect opts

isMultiple :: [a] -> Bool
isMultiple []   = False
isMultiple [_]  = False
isMultiple _    = True

parseCommand :: (String, [String]) -> IO ([Option], [String])
parseCommand (prog, argv) =
    case getOpt Permute options argv of
      (opts, _, [])
          | has OptHelp    -> putSuccess usage
          | has OptVersion -> putSuccess version
          where has = (`elem` opts)
                version = "koshu-rdf-" ++ versionString
      (opts, [], [])       -> return (opts, ["-"])
      (opts, files, [])    -> return (opts, files)
      (_, _, errs)         -> putFailure $ concat errs
    where
      usage  = usageInfo header options
      header = unlines
               [ "USAGE: " ++ prog ++ " [option ...] [file ...]"
               , "  Convert RDF to Koshucode."
               , "  If no files are given, read standard input."
               , ""
               ] ++ "OPTIONS:"



-- ----------------------  Conversion

convert :: [Option] -> FilePath -> IO ()
convert opts path
    | OptXml `elem` opts = writeRdf opts $ readXmlRdf path
    | otherwise          = writeRdf opts $ readRdfGraph turtleParser path

type Graph = RDF.MGraph

writeRdf :: [Option] -> IO Graph -> IO ()
writeRdf opts graph = do
  g <- graph
  let js = judgesFromRdf (tupleType opts) g
  writeJudges js

tupleType :: [Option] -> RDFTupleType
tupleType opts
    | Opt3 `elem` opts = RDFTuple3
    | otherwise        = RDFTuple2

readRdfGraph :: (RDF.RdfParser p) => p -> String -> IO Graph
readRdfGraph parser "-" = do
  str <- getContents
  let rdf = RDF.parseString parser $ Text.pack str
  return $ RDF.fromEither rdf
readRdfGraph parser path = do
  rdf <- RDF.parseFile parser path
  return $ RDF.fromEither rdf

readXmlRdf :: FilePath -> IO Graph
readXmlRdf "-" = do
  str <- getContents
  let rdf = xmlParser $ Text.pack str
  return $ RDF.fromEither rdf
readXmlRdf path = do
  txt <- Text.readFile path
  let rdf = xmlParser txt
  return $ RDF.fromEither rdf

writeJudges :: [Judge Val] -> IO ()
writeJudges js = print $ docv js



-- ----------------------  Parsers

turtleParser :: RDF.TurtleParser
turtleParser = RDF.TurtleParser Nothing Nothing

xmlParser :: (RDF.RDF g) => Text.Text -> Either RDF.ParseFailure g
xmlParser = RDF.parseXmlRDF Nothing Nothing


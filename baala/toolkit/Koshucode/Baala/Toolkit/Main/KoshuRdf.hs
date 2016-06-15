{-# OPTIONS_GHC -Wall #-}

-- | RDF-to-Koshucode converter.

module Koshucode.Baala.Toolkit.Main.KoshuRdf
  ( koshuRdfMain
    -- * koshu-rdf.hs
    -- $koshu-rdf.hs
  ) where

import qualified Data.List                as List
import qualified Data.Map                 as Map
import qualified Data.RDF                 as RDF
import qualified Data.Text                as Tx
import System.Console.GetOpt

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core                    as C
import qualified Koshucode.Baala.Toolkit.Library.RDF     as L
import qualified Koshucode.Baala.Toolkit.Library.Version as L



-- ----------------------  Main

koshuRdfMain :: IO ()
koshuRdfMain = do
  (opts, files) <- parseCommand =<< B.progAndArgs
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
  B.putFailure "choose one of -n -t -x"
checkOptions opts | more opts [Opt2, Opt3] =
  B.putFailure "choose one of -2 -3"
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
          | has OptHelp    -> B.putSuccess usage
          | has OptVersion -> B.putSuccess version
          where has = (`elem` opts)
                version = "koshu-rdf-" ++ L.versionString
      (opts, [], [])       -> return (opts, ["-"])
      (opts, files, [])    -> return (opts, files)
      (_, _, errs)         -> B.putFailure $ concat errs
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
    | OptXml `elem` opts = write RDF.XmlParser
    | otherwise          = write RDF.TurtleParser
    where
      write parser = writeRdf opts $ readRdfGraph (parser Nothing Nothing) path

type Graph = RDF.MGraph

writeRdf :: [Option] -> IO Graph -> IO ()
writeRdf opts graph =
  do g <- graph
     let js  = L.judgesFromRdf (tupleType opts) g
         RDF.PrefixMappings pmap = RDF.prefixMappings g
         pre = map unpackPair $ Map.toList pmap
     writeJudges pre js

unpackPair :: (Tx.Text, Tx.Text) -> (String, String)
unpackPair (a, b) = (Tx.unpack a, Tx.unpack b)

tupleType :: [Option] -> L.RDFTupleType
tupleType opts
    | Opt2 `elem` opts = L.RDFTuple2
    | otherwise        = L.RDFTuple3

readRdfGraph :: (RDF.RdfParser p) => p -> String -> IO Graph
readRdfGraph parser "-" =
    do str <- getContents
       let rdf = RDF.parseString parser $ Tx.pack str
       return $ RDF.fromEither rdf
readRdfGraph parser path =
    do rdf <- RDF.parseFile parser path
       return $ RDF.fromEither rdf

writeJudges :: [S.ShortDef] -> [C.JudgeC] -> IO ()
writeJudges sh js =
    do putStrLn B.emacsModeComment
       putStrLn ""
       writeShort sh
       print $ B.docv $ map (D.judgeToStringShort $ S.shortText sh) js

writeShort :: [S.ShortDef] -> IO ()
writeShort [] = return ()
writeShort sh =
    do putStrLn "short"
       mapM_ putShort sh
       putStrLn ""
    where
      putShort (short, long) = putStrLn $ "  " ++ short ++ " " ++ show long



-- ----------------------
-- $koshu-rdf.hs
--
-- @koshu-rdf@ command is implemented using 'koshuRdfMain'.
--
-- > import Koshucode.Baala.Toolkit.Main.KoshuRdf
-- > 
-- > main :: IO ()
-- > main = koshuRdfMain


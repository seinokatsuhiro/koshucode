{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Print text assertions.

module Main (main) where

import qualified Koshucode.Baala.System            as O
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S
import qualified Koshucode.Baala.System.CliParser  as Z
import qualified Paths_koshucode_baala_toolkit     as V

import Koshucode.Baala.Syntax.Token.Pattern

-- --------------------------------------------  Parameter

programVersion :: String
programVersion = "koshu-text-assert-" ++ Z.showVersion V.version

optionHead :: [String]
optionHead =
    [ "DESCRIPTION"
    , "  Print text assertions."
    , ""
    , "USAGE"
    , "  koshu-text-assert [OPTION] FILE ..."
    , ""
    ]

options :: [Z.Option]
options = [Z.help, Z.version]


-- --------------------------------------------  Main

main :: IO ()
main =
  do rslt <- Z.parseCommand options
     case rslt of
       Left errs -> O.putAbortWith $ concat errs
       Right (z, args)
           | flag "help"     -> Z.printHelp optionHead options
           | flag "version"  -> O.putSuccessLn programVersion
           | otherwise       -> run clauseMix `mapM_` args
           where flag = Z.getFlag z

run :: ClauseMix -> FilePath -> IO ()
run f = readRun g where
    g = B.putMix B.crlfBreak . clauseMixForBz f

readRun :: (B.Bz -> IO()) -> FilePath -> IO ()
readRun f path =
    do file <- B.readBzFile path
       bz   <- B.abortLeft $ B.bzFileContent file
       f bz

                    
-- --------------------------------------------  Clause

type ClauseMix = S.TokenClause -> B.MixText

clauseMixForBz :: ClauseMix -> B.Bz -> B.MixText
clauseMixForBz f bz = B.mixLines texts where
    texts   = f <$> clauses
    clauses = S.tokenClauses $ S.tokenLinesTextAssert B.def bz

pattern Affirm name ts <- (TBar "|==") : (TRaw name) : (TRaw ":") : ts

clauseMix :: S.TokenClause -> B.MixText
clauseMix = dispatch . B.clauseTokens where
    dispatch (Affirm name ts) = B.mix name `B.mixSep` B.mix ":" `B.mixSep` tokensMix ts
    dispatch ts = B.mix "**" `B.mixSep` B.mixShow ts

tokensMix :: [S.Token] -> B.MixText
tokensMix (t : ts)  = tokenMix t `B.mixSep` tokensMix ts
tokensMix []        = B.mixEmpty

tokenMix :: S.Token -> B.MixText
tokenMix (TRaw s)  = B.mix s
tokenMix t         = B.mixShow t

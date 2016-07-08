{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Print text assertions.

module Main (main) where

import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Syntax           as S
import qualified Paths_koshucode_baala_toolkit    as V


-- --------------------------------------------  Parameter

programVersion :: String
programVersion = "koshu-text-assert-" ++ B.showVersion V.version

optionHead :: [String]
optionHead =
    [ "DESCRIPTION"
    , "  Print text assertions."
    , ""
    , "USAGE"
    , "  koshu-text-assert [OPTION] FILE ..."
    , ""
    ]

options :: B.SimpleOptions
options = [B.help, B.version]


-- --------------------------------------------  Main

main :: IO ()
main =
  do rslt <- B.parseCommand options
     case rslt of
       Left errs -> B.putAbortWith $ concat errs
       Right (opts, args)
           | flag "help"     -> B.printHelp optionHead options
           | flag "version"  -> B.putSuccessLn programVersion
           | otherwise       -> run clauseMix `mapM_` args
           where flag = B.getFlag opts

run :: ClauseMix -> FilePath -> IO ()
run f = readRun g where
    g = B.putMix B.crlfBreak . clauseMixForBz f

readRun :: (B.Bz -> IO()) -> FilePath -> IO ()
readRun f path =
    do file <- B.readBzFile path
       case B.bzFileException file of
         Just e  -> B.putAbortWith $ show e
         Nothing -> f $ B.bzFileContent file

                    
-- --------------------------------------------  Clause

type ClauseMix = S.TokenClause -> B.MixText

clauseMixForBz :: ClauseMix -> B.Bz -> B.MixText
clauseMixForBz f bz = B.mixLines texts where
    texts   = f <$> clauses
    clauses = S.tokenClauses $ S.tokenLinesBzTextAssert B.def bz

pattern Affirm name ts <-
    (S.TTextBar _ "|==") : (S.TTextRaw _ name) : (S.TTextRaw _ ":") : ts

clauseMix :: S.TokenClause -> B.MixText
clauseMix = dispatch . B.clauseTokens where
    dispatch (Affirm name ts) = B.mix name `B.mixSep` B.mix ":" `B.mixSep` tokensMix ts
    dispatch ts = B.mix "**" `B.mixSep` B.mixShow ts

tokensMix :: [S.Token] -> B.MixText
tokensMix (t : ts)  = tokenMix t `B.mixSep` tokensMix ts
tokensMix []        = B.mixEmpty

tokenMix :: S.Token -> B.MixText
tokenMix (S.TTextRaw _ s) = B.mix s
tokenMix t                = B.mixShow t
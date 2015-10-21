{-# OPTIONS_GHC -Wall #-}

-- | Input sources.

module Koshucode.Baala.Toolkit.Library.Input
  ( -- * Input
    Input (..),
    inputText,
    readInput,
    readInputs,
    readJudge,
  ) where

import qualified Koshucode.Baala.Core  as C

data Input = Stdin | File FilePath
             deriving (Show, Eq)

inputText :: Input -> String
inputText (Stdin)   = "<stdin>"
inputText (File p)  = p

readInput :: Input -> IO String
readInput (Stdin)   = getContents
readInput (File p)  = readFile p

readInputs :: [Input] -> IO [String]
readInputs = mapM readInput

readJudge :: String -> [C.JudgeC]
readJudge src =
    let root = C.resEmpty :: C.ResourceC
    in case C.readResourceText root src of
         Right sec  -> C.resJudge sec
         Left _     -> []


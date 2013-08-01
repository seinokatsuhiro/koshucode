{-# OPTIONS_GHC -Wall #-}

{-| Input sources. -}

module Koshucode.Baala.Toolkit.Library.Input
(
-- * Input
  Input (..)
, inputText
, readInput
, readInputs
, readJudge
) where

import Koshucode.Baala.Base
import Koshucode.Baala.Core
import Koshucode.Baala.Vanilla


-- ----------------------  Input

data Input = Stdin | File FilePath
             deriving (Show, Eq)

inputText :: Input -> String
inputText (Stdin)  = "<stdin>"
inputText (File p) = p

readInput :: Input -> IO String
readInput (Stdin)  = getContents
readInput (File p) = readFile p

readInputs :: [Input] -> IO [String]
readInputs = mapM readInput

readJudge :: String -> [Judge VContent]
readJudge s =
    let root = emptySection :: Section VContent
    in case sectionRead root s of
         Right sec -> sectionJudge sec
         Left _    -> []


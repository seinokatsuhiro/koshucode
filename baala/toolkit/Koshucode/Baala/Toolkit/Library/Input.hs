{-# OPTIONS_GHC -Wall #-}

-- | Input resources.

module Koshucode.Baala.Toolkit.Library.Input
  ( -- * Input
    Input (..),
    inputText,
    readInput,
    readInputs,
    readJudge,
  ) where

import qualified Koshucode.Baala.Base  as B
import qualified Koshucode.Baala.Data  as D
import qualified Koshucode.Baala.Core  as C

-- | Input resource.
data Input = Stdin | File FilePath
             deriving (Show, Eq)

-- | Input name.
inputText :: Input -> String
inputText (Stdin)   = "<stdin>"
inputText (File p)  = p

-- | Read input resource.
readInput :: Input -> IO String
readInput (Stdin)   = getContents
readInput (File p)  = readFile p

-- | Read multiple input resources.
readInputs :: [Input] -> IO [String]
readInputs = mapM readInput

-- | Read input code and extract judges.
readJudge :: String -> [D.JudgeC]
readJudge code =
    case C.resReadBytes B.def $ B.stringBz code of
      Right res  -> C.resJudge res
      Left _     -> []


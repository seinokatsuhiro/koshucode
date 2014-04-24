{-# OPTIONS_GHC -Wall #-}

{-| Input sources. -}

module Koshucode.Baala.Toolkit.Library.Input
(
  -- * Input
  Input (..),
  inputText,
  readInput,
  readInputs,
  readJudge,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Op   as Op


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

readJudge :: String -> [B.Judge Op.VContent]
readJudge src =
    let root = C.emptySection :: C.Section Op.VContent
    in case C.readSectionText root src of
         Right sec -> C.secJudge sec
         Left _    -> []


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Section.Letter
( Letter (..),
  appendSection,
  appendSections,
  runLetter,
  hPutLetter,
) where

import qualified System.IO                            as IO
import qualified Data.Monoid                          as M
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Section.Section as C
import qualified Koshucode.Baala.Core.Section.Process as C

data Letter c =
    Letter (Maybe String) [C.Section c]
    deriving (Show)

appendSection :: Letter c -> C.Section c -> Letter c
appendSection (Letter name ss) s = Letter name $ s : ss

appendSections :: Letter c -> [C.Section c] -> Letter c
appendSections (Letter name s1) s2 = Letter name $ s1 ++ s2

runLetter :: (C.CContent c) => C.Global c -> B.AbMap (Letter c)
runLetter global (Letter name s1) =
    do let s2 = M.mconcat s1
           g2 = global { C.globalJudges = C.sectionJudge s2 }
       output <- runSection g2 `mapM` s1
       Right $ Letter name output

runSection :: (C.CNil c, Ord c) => C.Global c -> B.AbMap (C.Section c)
runSection global sec =
    do let (normal, violated) = C.sectionLinkedAssert sec
           run = C.runAssertJudges global
       judgesV <- run violated
       judgesN <- run normal
       Right $ sec { C.sectionJudge   = judgesN
                   , C.sectionViolate = judgesV }

hPutLetter :: (C.CContent c) => IO.Handle -> Letter c -> IO Int
hPutLetter h (Letter _ s1) =
    let violated = concatMap C.sectionViolate s1
        normal   = concatMap C.sectionJudge s1
    in if violated /= []
       then B.hPutJudges h 1 violated
       else B.hPutJudges h 0 normal


#!/usr/bin/env runhaskell
--
--  DESCRIPTION
--    Convert ISO-639-2 code list to Koshucode.
--
--  USAGE
--    chmod 755 to-koshu.hs
--    ./to-koshu.hs < ISO-639-2.txt | koshu -i decompose.k > ISO-639-2.k
--

{-# OPTIONS_GHC -Wall #-}

import Koshucode.Baala.Base
import Koshucode.Baala.Vanilla

main :: IO ()
main =
    do src <- getContents
       let ls  = lines src
           ls' = map (divideBy '|') ls
           js  = map toJudge ls'
       putJudges js

toJudge :: [String] -> Judge VContent
toJudge [alpha3bib, alpha3term, alpha2, english, french] =
    Judge True "ISO-639-2"
              [ ("/alpha3"  , putText alpha3bib)
              , ("/alpha3t" , putText alpha3term)
              , ("/alpha2"  , putText alpha2)
              , ("/english" , putText english)
              , ("/french"  , putText french) ]
toJudge xs = error $ "FORMAT ERROR: " ++ show xs


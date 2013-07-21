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
              [ ("/alpha3"  , putString alpha3bib)
              , ("/alpha3t" , putString alpha3term)
              , ("/alpha2"  , putString alpha2)
              , ("/english" , putString english)
              , ("/french"  , putString french) ]
toJudge xs = error $ "FORMAT ERROR: " ++ show xs


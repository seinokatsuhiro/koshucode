#!/usr/bin/env runhaskell
--
--  DESCRIPTION
--    Convert ISO-639-2 code list to Koshucode.
--
--  USAGE
--    chmod 755 to-koshu.hs
--    ./to-koshu.hs < ISO-639-2.txt > ISO-639-2.k
--

{-# OPTIONS_GHC -Wall #-}

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Vanilla.Value.Val

main :: IO ()
main =
    do src <- getContents
       let ls  = lines src
           ls' = map (divideBy '|') ls
           js  = map toJudge ls'
       putJudges js

toJudge :: [String] -> Judge Val
toJudge [alpha3bib, alpha3term, alpha2, english, french] =
    Judge True "ISO-639-2" [ ("/alpha3"  , Stringv alpha3bib)
                           , ("/alpha3t" , Stringv alpha3term)
                           , ("/alpha2"  , Stringv alpha2)
                           , ("/english" , Stringv english)
                           , ("/french"  , Stringv french) ]
toJudge xs = error $ "FORMAT ERROR: " ++ show xs


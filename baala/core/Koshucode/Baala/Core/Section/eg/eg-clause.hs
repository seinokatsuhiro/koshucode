#!/usr/bin/env runhaskell
--
--  DESCRIPTION
--    Examples of clauses
--
--  USAGE
--    chmod 755 eg-clause.hs
--    ./eg-clause.hs > eg-clause.log
--

{-# OPTIONS_GHC -Wall #-}

import Koshucode.Baala.Base
import Koshucode.Baala.Core



-- ----------------------  Utility

(>>>) :: (Show a) => String -> a -> IO ()
(>>>) n x =
    let len  = length n
        dots = concat $ repeat " . "
        fill = take (20 - len) dots
    in  putStrLn $ n ++ " " ++ fill ++ " " ++ show x

(>>>>) :: (Show a) => String -> a -> IO ()
(>>>>) n x =
    do n >>> x
       putStrLn ""

eg :: IO () -> IO ()
eg f = do f
          putStrLn ""

clause :: String -> [Clause]
clause = consPreclause . tokenize



-- ----------------------  Main

main :: IO ()
main =
    do eg egClause

egClause :: IO ()
egClause =
  do "section"     >>>> clause "section 'http://example.com/'"
     "import"      >>>> clause "import 'http://example.com/'"
     "export"      >>>> clause "export aa"
     "judge"       >>>> clause "|-- A /x 0 /y 0"
     "relmap"      >>>> clause "a : source A /x /y"
     "relmap"      >>>> clause "a : @a"


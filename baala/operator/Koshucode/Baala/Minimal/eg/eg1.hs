#!/usr/bin/env runhaskell
--
--  DESCRIPTION
--    Examples
--
--  USAGE
--    chmod 755 eg1.hs
--    ./eg1.hs > eg1.log
--

{-# OPTIONS_GHC -Wall #-}

import Koshucode.Baala.Base
import Koshucode.Baala.Builtin



-- ----------------------  Utility

(>>>) :: (Show a) => String -> a -> IO ()
(>>>) n x = let len  = length n
                dots = concat $ repeat " . "
                fill = take (20 - len) dots
            in  putStrLn $ n ++ " " ++ fill ++ " " ++ show x

eg :: IO () -> IO ()
eg f = do f
          putStrLn ""

trees :: String -> [TokenTree]
trees = tokenTrees . tokens



-- ----------------------  Main

main :: IO ()
main =
  do eg egTermNames
     eg egTermNamePairs
     eg egTermTreePairs

egTermNames :: IO ()
egTermNames =
    do "termnames"     >>> e ""
       "termnames"     >>> e "/a /b /c"
       "termnames"     >>> e "/a /b \n /c"
       "termnames"     >>> e "/a bb /c"
    where
      e = termnames . Main.trees

egTermNamePairs :: IO ()
egTermNamePairs =
    do "termnamePairs"     >>> e ""
       "termnamePairs"     >>> e "/a /x"
       "termnamePairs"     >>> e "/a /x  /b /y"
       "termnamePairs"     >>> e "/a /x  /b /y  /c"
       "termnamePairs"     >>> e "/a /x  (/b /y)"
    where
      e = termnamePairs . Main.trees

egTermTreePairs :: IO ()
egTermTreePairs =
    do "termTreePairs"     >>> e ""
       "termTreePairs"     >>> e "/a /x"
       "termTreePairs"     >>> e "/a (/x + 1)  /b /y"
       "termTreePairs"     >>> e "/a (/x + 1)  /b"
    where
      e = termTreePairs . Main.trees


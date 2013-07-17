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

import Koshucode.Baala.Base.Syntax
import Koshucode.Baala.Minimal.Term



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
    do "termNames"     >>> e ""
       "termNames"     >>> e "/a /b /c"
       "termNames"     >>> e "/a /b \n /c"
       "termNames"     >>> e "/a bb /c"
    where
      e = termNames . Main.trees

egTermNamePairs :: IO ()
egTermNamePairs =
    do "termNamePairs"     >>> e ""
       "termNamePairs"     >>> e "/a /x"
       "termNamePairs"     >>> e "/a /x  /b /y"
       "termNamePairs"     >>> e "/a /x  /b /y  /c"
       "termNamePairs"     >>> e "/a /x  (/b /y)"
    where
      e = termNamePairs . Main.trees

egTermTreePairs :: IO ()
egTermTreePairs =
    do "termTreePairs"     >>> e ""
       "termTreePairs"     >>> e "/a /x"
       "termTreePairs"     >>> e "/a (/x + 1)  /b /y"
       "termTreePairs"     >>> e "/a (/x + 1)  /b"
    where
      e = termTreePairs . Main.trees


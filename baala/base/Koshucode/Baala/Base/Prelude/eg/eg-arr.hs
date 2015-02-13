#!/usr/bin/env runhaskell
--
--  DESCRIPTION
--    Examples of arrangement
--
--  USAGE
--    regress.sh eg-arr.hs -l   # Execute regression test
--    regress.sh eg-arr.hs -s   # Save last result
--

{-# OPTIONS_GHC -Wall #-}

import Koshucode.Baala.Base



-- ----------------------  Utility

(>>>) :: (Show a) => String -> a -> IO ()
(>>>) n x = let len  = length n
                dots = concat $ repeat " . "
                fill = take (20 - len) dots
            in  putStrLn $ n ++ " " ++ fill ++ " " ++ show x

eg :: IO () -> IO ()
eg f = do f
          putStrLn ""



-- ----------------------  Main

main :: IO ()
main =
    do eg egArrange

egArrange :: IO ()
egArrange =
    do "arrange"   >>> arr "abcdefg" [1,3]
       "arrange"   >>> arr "abcdefg" [5,4,1]
       "arrange"   >>> arr "abcdefg" [1,2,3]
       "arrange"   >>> arr "ab" [0,1]
       "arrange"   >>> arr "ab" [1,0]
       "arrange"   >>> arr "ab" [0,5]
       "arrange"   >>> arr "ab" [0]
       "arrange"   >>> arr "ab" [1]
       "arrange"   >>> arr "ab" [2]

arr :: [a] -> [Int] -> ([a], [a], [a], [Int])
arr x p = ( snipFrom p x
          , snipOff  p x
          , snipFore p x
          , p )


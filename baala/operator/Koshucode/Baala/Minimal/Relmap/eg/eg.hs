#!/usr/bin/env runhaskell
--
--  DESCRIPTION
--    Examples
--
--  USAGE
--    chmod 755 eg.hs
--    ./eg.hs > eg.log
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
    do eg egHead

{- Heading position

   R1 /a /b /c      R2 /b /x
       0  1  2          0  1

   from R2          from R1
      /a /b /c         /b /x
      -1  0 -1          1 -1

-}
egHead :: IO ()
egHead =
    do let head1  = headFrom $ words "/a /b /c"
           head2  = headFrom $ words "/b /x"

           from2  = head1 `posFrom` head2
           from1  = head2 `posFrom` head1

           inner  = termsInner from2
           outer  = termsOuter from2

           inner1 = headPoss head1 inner
           inner2 = headPoss head2 inner

       "from2"     >>> from2
       "from1"     >>> from1

       "inner"     >>> inner
       "outer"     >>> outer

       "inner1"    >>> inner1
       "inner2"    >>> inner2

       "pick"      >>> headChange (possPick inner1) head1


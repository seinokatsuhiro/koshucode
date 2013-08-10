#!/usr/bin/env runhaskell
--
--  DESCRIPTION
--    Examples of literal constants
--
--  USAGE
--    chmod 755 eg-lit.hs
--    regress.sh eg-lit.hs -l
--

{-# OPTIONS_GHC -Wall #-}

import Koshucode.Baala.Base
import Koshucode.Baala.Core
import Koshucode.Baala.Vanilla



-- ----------------------  Utility

(>>>) :: (Show a) => String -> a -> IO ()
(>>>) n x = let len  = length n
                dots = concat $ repeat " . "
                fill = take (20 - len) dots
            in  putStrLn $ n ++ " " ++ fill ++ " " ++ show x

eg :: IO () -> IO ()
eg f = do f
          putStrLn ""

lit :: String -> Ab [VContent]
lit = mapM litContent . tokenTrees . tokens



-- ----------------------  Main

main :: IO ()
main =
    do eg egSimple
       eg egCompound

egSimple :: IO ()
egSimple =
  do "boolean"     >>> lit "#true #false"
     "word"        >>> lit "a 'b c'"
     "word"        >>> lit "a 1"
     "integer"     >>> lit "(int 12)"
     "nil"         >>> lit "()"

egCompound :: IO ()
egCompound =
  do "set"         >>> lit "{ b a a c a }"
     "list"        >>> lit "[ a 10 (int 20) ]"
     "termset"     >>> lit "{| /a 0 /b [ a 1 ] |}"
     "relation"    >>> lit "[| /a /x | A1 (int 20) | A3 (int 40) | A4 (int 60) |]"


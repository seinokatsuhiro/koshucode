#!/usr/bin/env runhaskell
--
--  DESCRIPTION
--    Examples of tokenizer
--
--  USAGE
--    chmod 755 eg-token.hs
--    ./eg-token.hs > eg-token.log
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

fromLines :: [String] -> [Token]
fromLines = tokens . unlines


-- ----------------------  Main

main :: IO ()
main =
    do eg egWord
       eg egTerms
       eg egComment
       eg egParen

egWord :: IO ()
egWord =
  do "word"      >>> tokens "aa bb"
     "word"      >>> tokens "'aa' '' \"cc\""
     "word"      >>> tokens "aa (bb (cc))"

egTerms :: IO ()
egTerms =
  do "terms"     >>> tokens "|-- rel /a A0 /b 31"
     "terms"     >>> tokens "count /r/x/t"
     "terms"     >>> tokens "///x /r/"

egComment :: IO ()
egComment =
  do "comment"   >>> fromLines ["www", "** ccc", "www"]
     "comment"   >>> fromLines ["www", "**-", "  ccc", "-**", "www"]
     "comment"   >>> fromLines ["www", "**=", "  ccc", "=**", "www"]

egParen :: IO ()
egParen =
  do "paren"     >>> tokens "( a b )"    -- group
     "paren"     >>> tokens "{ a b }"    -- set
     "paren"     >>> tokens "[ a b ]"    -- list
     "paren"     >>> tokens "< a b >"    -- not parens, just word
     "paren"     >>> tokens "<| a b |>"  -- tuple
     "paren"     >>> tokens "{| a b |}"  -- relation
     "paren"     >>> tokens "[| a b |]"
     "paren"     >>> tokens "(| a b |)"


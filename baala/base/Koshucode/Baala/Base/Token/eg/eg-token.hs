#!/usr/bin/env koshu-env.sh runhaskell
{-# OPTIONS_GHC -Wall #-}
-- --------------------------------------------
--
--  DESCRIPTION
--    Examples of tokenizer
--
--  USAGE
--    chmod 755 eg-token.hs
--    ./eg-token.hs > eg-token.log
--
-- --------------------------------------------

import qualified Koshucode.Baala.Base as B



-- ----------------------  Utility

(>>>) :: (Show a) => String -> a -> IO ()
(>>>) n x = let len  = length n
                dots = concat $ repeat " . "
                fill = take (20 - len) dots
            in  putStrLn $ n ++ " " ++ fill ++ " " ++ show x

eg :: IO () -> IO ()
eg f = do f
          putStrLn ""

fromLines :: [String] -> [B.Token]
fromLines = B.tokens . unlines


-- ----------------------  Main

main :: IO ()
main =
    do eg egWord
       eg egTerms
       eg egComment
       eg egParen

egWord :: IO ()
egWord =
    do "word"      >>> B.tokens "aa bb"
       "word"      >>> B.tokens "'aa' '' \"cc\""
       "word"      >>> B.tokens "aa (bb (cc))"

egTerms :: IO ()
egTerms =
    do "terms"     >>> B.tokens "|-- rel /a A0 /b 31"
       "terms"     >>> B.tokens "count /r/x/t"
       "terms"     >>> B.tokens "///x /r/"

egComment :: IO ()
egComment =
    do "comment"   >>> fromLines ["www", "** ccc", "www"]
       "comment"   >>> fromLines ["www", "**-", "  ccc", "-**", "www"]
       "comment"   >>> fromLines ["www", "**=", "  ccc", "=**", "www"]

egParen :: IO ()
egParen =
    do "paren"     >>> B.tokens "( a b )"    -- group
       "paren"     >>> B.tokens "{ a b }"    -- set
       "paren"     >>> B.tokens "[ a b ]"    -- list
       "paren"     >>> B.tokens "< a b >"    -- not parens, just word
       "paren"     >>> B.tokens "<| a b |>"  -- tuple
       "paren"     >>> B.tokens "{| a b |}"  -- relation
       "paren"     >>> B.tokens "[| a b |]"
       "paren"     >>> B.tokens "(| a b |)"


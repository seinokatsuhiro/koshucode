{-# OPTIONS_GHC -Wall #-}

-- | Tokenizer

module Koshucode.Baala.Base.Syntax.Tokenizer
(
  tokens
, untokens
, untoken
) where

import qualified Data.Char as C

import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Base.Syntax.Token

{-| Line number and contents. -}
data LineText = LineText Int String
      deriving (Show, Eq)

{-| Split string into list of tokens

    >>> tokens "|-- R  /a A0 /b 31"
    [Line 1 "|-- R  /a A0 /b 31",
     Word 0 "|--", Space 1, Word 0 "R", Space 2,
     TermN ["/a"], Space 1, Word 0 "A0", Space 1,
     TermN ["/b"], Space 1, Word 0 "31"]
  -}
tokens :: String -> [Token]
tokens = concat . gather token . numbering . linesCrLf where
    numbering = zipWith LineText [1..]

token :: [LineText] -> ([Token], [LineText])
token (LineText n line : ls) = (lineToken : toks, ls)
    where lineToken = Line $ SourceLine n line toks
          toks = gather tokenInLine line
token [] = error "token for empty list"

tokenInLine :: String -> (Token, String)
tokenInLine ccs =
    case ccs of
      ('*' : '*' : '*' : '*' : cs) -> (Word 0 "****", cs)
      ('*' : '*' : _)  -> commL ccs []
      ('#' : '!' : _)  -> commL ccs []
      ('(' : ')' : cs) -> (Word 0 "()", cs) -- nil
      (c : cs)
        | isOpen    c  -> (Open   [c] , cs)
        | isClose   c  -> (Close  [c] , cs)
        | isSingle  c  -> (Word 0 [c] , cs)
        | c == '/'     -> term cs [c]  []
        | isQuote c    -> quote   c cs []
        | isWord c     -> word    ccs  []
        | isSpace c    -> white 1 cs
      _                -> error $ "unknown token type: " ++ ccs
    where
      -- collect chars
      tk tokenType xs cs = (tokenType $ reverse xs, cs)

      -- line comment
      commL (c:cs) xs | isMidline c  = commL cs (c:xs)
      commL ccs2 xs                  = tk Comment xs $ ccs2

      word (c:cs) xs | isWord c      = word cs (c:xs)
      word ccs2 xs                   = tk (Word 0) xs $ ccs2

      quote q [] xs                  = tk (quoteWord q) xs $ []
      quote q (c:cs) xs
          | c == q                   = tk (quoteWord q) xs $ cs
          | otherwise                = quote q cs (c:xs)

      quoteWord '\'' = Word 1
      quoteWord '"'  = Word 2
      quoteWord _    = Word 0

      term (c:cs) xs ns | c == '/'   = term cs [c] $ t xs ns
                        | isWord c   = term cs (c:xs) ns
      term ccs2 xs ns                = tk TermN (t xs ns) $ ccs2
      t xs = (reverse xs :)

      white n (c:cs)    | isSpace c  = white (n+1) cs
      white n ccs2                   = (Space n, ccs2)

-- word
-- e1 = tokens "aa bb"
-- e2 = tokens "'aa' '' \"cc\""
-- e3 = tokens "aa (bb (cc))"

-- terms
-- e1 = tokens "|-- rel /a A0 /b 31"
-- e2 = tokens "count /r/x/t"
-- e3 = tokens "///x /r/"

-- comment
-- e0 = tokens . unlines
-- e1 = e0 ["www", "** ccc", "www"]
-- e2 = e0 ["www", "**-", "  ccc", "-**", "www"]
-- e3 = e0 ["www", "**=", "  ccc", "=**", "www"]



-- ----------------------  Char category

isOpen, isClose, isSingle, isQuote, isNewline :: Char -> Bool
isMidline, isSpace, isWord, isNonWord, maybeWord :: Char -> Bool

isOpen    c  = c `elem` "([{"
isClose   c  = c `elem` "}])"
isSingle  c  = c `elem` ":="
isQuote   c  = c `elem` "'\""
isNewline c  = c == '\n'
isMidline    = not . isNewline
isSpace   c  = C.isSpace c && not (isNewline c)
isWord    c  = maybeWord c && not (isNonWord c)
isNonWord c  = isOpen c || isClose c || isSingle c
maybeWord c  = C.isAlphaNum c
               || C.isMark c
               || C.isSymbol c
               || C.isPunctuation c



-- ----------------------  Untokenizer

{-| Convert back a token list to a source string. -}
untokens :: [Token] -> String
untokens (Open a : xs) = a ++ untokens xs
untokens (x : Close a : xs) = untoken x ++ untokens (Close a : xs)
untokens [x] = untoken x
untokens (x:xs) = untoken x ++ untokens xs
untokens [] = []

untoken :: Token -> String
untoken (Word 1 s)   = "'" ++ s ++ "'"
untoken (Word 2 s)   = "\"" ++ s ++ "\""
untoken (Word _ s)   = s
untoken (TermN  ns)  = concat ns
untoken (TermP  ns)  = concatMap show ns
untoken (Open   s)   = s
untoken (Close  s)   = s
untoken (Space  n)   = replicate n ' '
untoken (Comment s)  = s
untoken (Line (SourceLine _ s _)) = s




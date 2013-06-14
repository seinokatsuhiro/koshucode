{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokens in Koshucode

module Koshucode.Baala.Base.Syntax.Token
(
  -- * Token type
  -- $TokenType
  Token (..)
, isBlank
, isLineToken
, isTerm

  -- * Tokenizer
, tokens
, untokens

  -- * Othre functions
, sweepToken
, sweepLeft
) where

import Data.Generics (Data, Typeable)
import Koshucode.Baala.Base.Prelude
import qualified Data.Char as C

data Token
    = Word Int String   -- ^ Text
    | TermN   [String]  -- ^ Term name
    | TermP   [Int]     -- ^ Term position
    | Open    String    -- ^ Open paren
    | Close   String    -- ^ Close paren
    | Space   Int       -- ^ N space chars
    | Comment String    -- ^ Comment text
    | Line SourceLine   -- ^ Source infomation
      deriving (Show, Eq, Ord, Data, Typeable)

instance Name Token where
    name (TermN  ns) = concat ns
    name (Word  _ s) = s
    name (Open    s) = s
    name (Close   s) = s
    name (Comment s) = s
    name x = error $ "unknown name: " ++ show x

-- | Test the token is blank, i.e.,
--   'Comment', 'Line', or 'Space'.
isBlank :: Token -> Bool
isBlank (Space _)    = True
isBlank (Comment _)  = True
isBlank (Line _)     = True
isBlank _            = False

isLineToken :: Token -> Bool
isLineToken (Line _)   = True
isLineToken _          = False

-- | Test the token is a term, i.e., 'TermN' or 'TermP'
isTerm :: Token -> Bool
isTerm (TermN _)     = True
isTerm (TermP _)     = True
isTerm _             = False



-- ----------------------  Tokenizer

data SrcLine
    = SrcLine Int String    -- ^ Line number and contents
    | MidLine String        -- ^ Subline
      deriving (Show, Eq)

-- | Split string into list of tokens
-- 
--   >>> tokens "|-- R  /a A0 /b 31"
--   [Line 1 "|-- R  /a A0 /b 31",
--    Word 0 "|--", Space 1, Word 0 "R", Space 2,
--    TermN ["/a"], Space 1, Word 0 "A0", Space 1,
--    TermN ["/b"], Space 1, Word 0 "31"]

tokens :: String -> [Token]
tokens = gather token . numbering . lines where
    numbering = zipWith SrcLine [1..]

token :: [SrcLine] -> (Token, [SrcLine])
token (SrcLine n line : ls) = tokenLines ls (Line $ SourceLine n line, line)
token (MidLine   line : ls) = tokenLines ls (tokenInLine line)
token [] = error "token for empty list"

tokenLines :: [SrcLine] -> (Token, String) -> (Token, [SrcLine])
tokenLines ls (tk, line)
    | line == ""  = (tk, ls)
    | otherwise   = (tk, MidLine line : ls)

tokenInLine :: String -> (Token, String)
tokenInLine ccs =
    case ccs of
      ('*' : '*' : c : _)
        | c == '-'     -> commB c ccs []
        | c == '='     -> commB c ccs []
        | otherwise    -> commL ccs [] 
      ('*' : '*' : _)  -> commL ccs []
      ('#' : '!' : _)  -> commL ccs []
      ('(' : ')' : cs) -> (Word 0 "()", cs) -- nil
      (c:cs)
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

      -- block comment
      commB _ [] xs                  = tk Comment xs $ []
      commB q (c : '*' : '*' : cs) xs
          | c == q                   = tk Comment ('*' : '*' : c : xs) $ cs
      commB q (c:cs) xs              = commB q cs (c:xs)

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

-- | Convert back a token list to a source string
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
untoken (Line (SourceLine _ s)) = s



-- ---------------------- Other functions

-- | Remove blank tokens.
sweepToken :: [Token] -> [Token]
sweepToken = filter (not . isBlank)

-- | Skip leading blank tokens.
sweepLeft :: [Token] -> [Token]
sweepLeft [] = []
sweepLeft xxs@(x:xs) | isBlank x = sweepLeft xs
                     | otherwise = xxs



-- ----------------------
-- $TokenType
--
-- [Word]
-- Character sequence not including special characters,
-- e.g., @aa@, @r2d2@, @12.0@, etc.
--
-- [Term name]
-- Word beginning with slash, e.g., @\/aa@.
-- Term name like @\/a\/b@ is used for nested relation,
-- that means term @\/b@ in the relation of term @\/a@.
--
-- [Paren]
-- Open and closed parens.
--
-- [Comment]
-- Text from @**@ to end of line is single line comment.
-- Text from @**-@ to @-**@, or from @**=@ to @=**@ is multiline comment.
--
-- [Space]
-- Space characters.
--
-- [Line]
-- Source line.


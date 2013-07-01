{-# OPTIONS_GHC -Wall #-}

-- | Tokenizer of koshucode.

module Koshucode.Baala.Base.Syntax.Tokenizer
(
-- * Tokenizer
  tokens
, untokens
, untoken
, sourceLines

-- * Asterisks
-- $Asterisks

-- * Process
-- $Process
) where

import qualified Data.Char as C

import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Base.Syntax.Token



-- ----------------------  Tokenizer

{-| Split string into list of tokens.

    Result token list does not contain newline characters.
    Insead of newlines, 'Line' tokens are there
    at the start of each lines.

    >>> tokens "aa\n'bb'\n\"cc\""
    [Line (SourceLine 1 "aa"     [Word 0 "aa"]), Word 0 "aa",
     Line (SourceLine 2 "'bb'"   [Word 1 "bb"]), Word 1 "bb",
     Line (SourceLine 3 "\"cc\"" [Word 2 "cc"]), Word 2 "cc"]

    Example includes 'Line', 'Word', 'TermN' and 'Space' tokens.

    >>> tokens "|-- R  /a A0 /b 31"
    [Line (SourceLine 1 "|-- R  /a A0 /b 31", [Word 0 "|--", ...]),
     Word 0 "|--", Space 1, Word 0 "R", Space 2,
     TermN ["/a"], Space 1, Word 0 "A0", Space 1,
     TermN ["/b"], Space 1, Word 0 "31"]
  -}
tokens
    :: String   -- ^ Input text in koshucode
    -> [Token]  -- ^ Token list from the input
tokens = concatMap (expand) . sourceLines where
    expand :: SourceLine -> [Token]
    expand s = Line s : sourceLineTokens s

{-| Split source text into 'SourceLine' list. -}
sourceLines :: String -> [SourceLine]
sourceLines = map sourceLine . linesNumbered

sourceLine :: (Int, String) -> SourceLine
sourceLine (n, ln) = SourceLine n ln $ gather nextToken ln

{-| Line number and contents. -}
linesNumbered :: String -> [(Int, String)]
linesNumbered = zip [1..] . linesCrLf

{-| Split a next token from source text. -}
nextToken :: String -> (Token, String)
nextToken ccs =
    case ccs of
      ('*' : '*' : '*' : '*' : cs) -> (Word 0 "****", cs)
      ('*' : '*' : _)   ->  (Comment ccs, "")
      ('#' : '!' : _)   ->  (Comment ccs, "")
      ('(' : ')' : cs)  ->  (Word 0 "()", cs) -- nil
      (c : cs)
        | isOpen    c   ->  (Open   [c] , cs)
        | isClose   c   ->  (Close  [c] , cs)
        | isSingle  c   ->  (Word 0 [c] , cs)
        | isTerm    c   ->  term cs [c]  []
        | isQuote   c   ->  quote   c cs []
        | isWord    c   ->  word    ccs  []
        | isSpace   c   ->  white 1 cs
      _                 ->  error $ "unknown token type: " ++ ccs
    where
      tok cons xs cs = (cons $ reverse xs, cs)

      word (c:cs) xs | isWord c      = word cs (c:xs)
      word ccs2 xs                   = tok (Word 0) xs $ ccs2

      quote q [] xs                  = tok (wordQ q) xs $ []
      quote q (c:cs) xs
          | c == q                   = tok (wordQ q) xs $ cs
          | otherwise                = quote q cs (c:xs)

      wordQ '\'' = Word 1
      wordQ '"'  = Word 2
      wordQ _    = Word 0

      term (c:cs) xs ns | isTerm c   = term cs [c] $ t xs ns
                        | isWord c   = term cs (c:xs) ns
      term ccs2 xs ns                = tok TermN (t xs ns) $ ccs2
      t xs = (reverse xs :)

      white n (c:cs)    | isSpace c  = white (n + 1) cs
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

isTerm, isOpen, isClose, isSingle, isQuote :: Char -> Bool
isSpace, isWord, isNonWord, maybeWord :: Char -> Bool

isTerm    c  =  c == '/'
isOpen    c  =  c `elem` "([{"
isClose   c  =  c `elem` "}])"
isSingle  c  =  c `elem` ":="
isQuote   c  =  c `elem` "'\""
isSpace   c  =  C.isSpace c
isWord    c  =  maybeWord c && not (isNonWord c)
isNonWord c  =  isOpen c || isClose c || isSingle c
maybeWord c  =  C.isAlphaNum c
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



-- ----------------------
-- $Asterisks
--
-- There are three uses of asterisks (@*@) in koshucode,
-- 
-- [@*@]
--  Single asterisk means multiplication,
--  e.g., @3 * 4@ (three times four).
--
-- [@**@]
--  Double asterisk leads line comment.
--  Textx from @**@ to end of line are ignored.
--
-- [@****@]
--  Quadruple asterisk (fourfold asterisk) leads caluse comment.
--  Texts from @****@ to end of clause are ignored.
--  In other words, line staring with @****@
--  and following indented lines are ignored.
--
-- Triple asterisk @***@ is double and rest of text.
-- Quintuple (fivefold) asterisk @*****@ is quadruple and rest of text.
--
-- Line comments like this:
--
-- @
-- ** aaa bbbbb cc
-- @
--
-- Clause comments like this:
--
-- @
-- **** aaa bbb
--      ccccc dddddd
--      ee fffff
-- @
--
-- You can type @****@ on top of a clause to hide it.



-- ----------------------
-- $Process
--
-- 1. Split source code into lines by line delimiters
--    (carriage return @\\r@ or line feed @\\n@)
--
-- 2. Numbering line numbers.
--    Internally, this is represented as
--    a list of pairs (/line#/, /content/).
--
-- 3. Tokenize each lines.
--    This is represented as a list of
--    'SourceLine' /line#/ /content/ /tokens/.
--
-- 4. Wrap 'SourceLine' into 'Line' token.
--
-- 5. Extract tokens from 'SourceLine',
--    and concat it like 'Line', /token/ ..., 'Line', /token/ ...


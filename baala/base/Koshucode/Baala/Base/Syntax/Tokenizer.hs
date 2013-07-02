{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokenizer of koshucode.

module Koshucode.Baala.Base.Syntax.Tokenizer
(
-- * Source lines
  SourceLine (..)
, sourceLines

-- * Tokenizer
, tokens
, untokens
, untoken

-- * Asterisks
-- $Asterisks
) where

import Data.Generics (Data, Typeable)
import qualified Data.Char as C

import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Base.Syntax.Token



-- ----------------------  Source line

data SourceLine = SourceLine
    { sourceLineNumber  :: Int     -- ^ Line number, from 1.
    , sourceLineContent :: String  -- ^ Line content without newline.
    , sourceLineTokens  :: [Token] -- ^ Tokens from line.
    } deriving (Show, Eq, Ord, Data, Typeable)

instance Pretty SourceLine where
    doc (SourceLine _ line _) = text line

{-| Split source text into 'SourceLine' list.

    1. Split source code into lines by line delimiters
       (carriage return @\\r@ or line feed @\\n@)

   2. Numbering line numbers.
      Internally, this is represented as
      a list of pairs (/line#/, /content/).

   3. Tokenize each lines.
      This is represented as a list of
      'SourceLine' /line#/ /content/ /tokens/.
  -}
sourceLines
    :: String        -- ^ Source text in Koshucode.
    -> [SourceLine]  -- ^ Token list per lines.
sourceLines = concatMap sourceLine . linesNumbered

sourceLine :: (Int, String) -> [SourceLine]
sourceLine (n, ln) = map src ls where
    toks = gather nextToken ln
    ls   = divideBy (TWord 0 "||") toks
    src  = SourceLine n ln

{-| Line number and contents. -}
linesNumbered :: String -> [(Int, String)]
linesNumbered = zip [1..] . linesCrLf

{-| Split string into lines.
    The result strings do not contain
    carriage returns (@\\r@)
    and line feeds (@\\n@). -}
linesCrLf :: String -> [String]
linesCrLf "" = []
linesCrLf s = ln : nextLine s2 where
    (ln, s2) = break (`elem` "\r\n") s
    nextLine ('\r' : s3) = nextLine s3
    nextLine ('\n' : s3) = nextLine s3
    nextLine s3 = linesCrLf s3



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
tokens :: String -> [Token]
tokens = concatMap sourceLineTokens . sourceLines

{-| Split a next token from source text. -}
nextToken :: String -> (Token, String)
nextToken ccs =
    case ccs of
      ('*' : '*' : '*' : '*' : cs) -> (TWord 0 "****", cs)
      ('*' : '*' : _)   ->  (TComment ccs, "")
      ('#' : '!' : _)   ->  (TComment ccs, "")
      ('(' : ')' : cs)  ->  (TWord 0 "()", cs) -- nil
      ('|' : '|' : cs)  ->  (TWord 0 "||", dropSpaces cs) -- newline
      (c : cs)
        | isOpen    c   ->  (TOpen   [c] , cs)
        | isClose   c   ->  (TClose  [c] , cs)
        | isSingle  c   ->  (TWord 0 [c] , cs)
        | isTerm    c   ->  term cs [c]  []
        | isQuote   c   ->  quote   c cs []
        | isWord    c   ->  word    ccs  []
        | isSpace   c   ->  white 1 cs
      _                 ->  error $ "unknown token type: " ++ ccs
    where
      dropSpaces     = dropWhile isSpace
      tok cons xs cs = (cons $ reverse xs, cs)

      word (c:cs) xs | isWord c      = word cs (c:xs)
      word ccs2 xs                   = tok (TWord 0) xs $ ccs2

      quote q [] xs                  = tok (wordQ q) xs $ []
      quote q (c:cs) xs
          | c == q                   = tok (wordQ q) xs $ cs
          | otherwise                = quote q cs (c:xs)

      wordQ '\'' = TWord 1
      wordQ '"'  = TWord 2
      wordQ _    = TWord 0

      term (c:cs) xs ns | isTerm c   = term cs [c] $ t xs ns
                        | isWord c   = term cs (c:xs) ns
      term ccs2 xs ns                = tok TTermN (t xs ns) $ ccs2
      t xs = (reverse xs :)

      white n (c:cs)    | isSpace c  = white (n + 1) cs
      white n ccs2                   = (TSpace n, ccs2)

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
untokens (TOpen a : xs)      = a ++ untokens xs
untokens (x : TClose a : xs) = untoken x ++ untokens (TClose a : xs)
untokens [x]    = untoken x
untokens (x:xs) = untoken x ++ untokens xs
untokens []     = []

untoken :: Token -> String
untoken (TWord 1 s)   = "'" ++ s ++ "'"
untoken (TWord 2 s)   = "\"" ++ s ++ "\""
untoken (TWord _ s)   = s
untoken (TTermN  ns)  = concat ns
untoken (TTermP  ns)  = concatMap show ns
untoken (TOpen   s)   = s
untoken (TClose  s)   = s
untoken (TSpace  n)   = replicate n ' '
untoken (TComment s)  = s



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


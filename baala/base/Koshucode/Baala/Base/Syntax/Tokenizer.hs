{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokenizer of koshucode.

module Koshucode.Baala.Base.Syntax.Tokenizer
(
-- * Library

  -- ** Source lines
  SourceLine (..)
, sourceLines

  -- ** Tokenizer
, tokens
, untokens
, untoken

-- * Document

  -- ** Special characters
  -- $SpecialCharacters

  -- ** Asterisks
  -- $Asterisks

  -- ** Escape sequences
  -- $EscapeSequences

  -- ** Syntactic content type
  -- $SyntacticContentType
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
sourceLines = loop 1 . linesNumbered where
    loop _ [] = []
    loop tok (p : ps) =
        case sourceLine tok p of
          (tok', src) -> src ++ loop tok' ps

sourceLine :: Int -> (Int, String) -> (Int, [SourceLine])
sourceLine tok (n, ln) = (tok + length toks, map src ls) where
    toks = gatherWith nextToken [tok ..] ln
    ls   = divideByP isDoubleBar toks
    src  = SourceLine n ln

    isDoubleBar (TWord _ 0 "||") = True
    isDoubleBar _                = False

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

    >>> tokens "aa\n'bb'\n\"cc\""
    [TWord 0 "aa", TWord 1 "bb", TWord 2 "cc"]

    Example includes 'TWord', 'TTermN' and 'TSpace' tokens.

    >>> tokens "|-- R  /a A0 /b 31"
    [TWord 0 "|--", TSpace 1, TWord 0 "R", TSpace 2,
     TTermN ["/a"], TSpace 1, TWord 0 "A0", TSpace 1,
     TTermN ["/b"], TSpace 1, TWord 0 "31"]
  -}
tokens :: String -> [Token]
tokens = concatMap sourceLineTokens . sourceLines

{-| Split a next token from source text. -}
nextToken :: Int -> String -> (Token, String)
nextToken n ccs =
    case ccs of
      ('*' : '*' : '*' : '*' : cs) -> tokD cs (TWord n 0 "****")
      ('*' : '*' : _)   ->  tokD "" (TComment n ccs)
      ('#' : '!' : _)   ->  tokD "" (TComment n ccs)
      ('(' : ')' : cs)  ->  tokD cs (TWord n 0 "()") -- nil
      ('|' : '|' : cs)  ->  tokD (dropSpaces cs) (TWord n 0 "||") -- newline
      (c : cs)
        | isOpen    c   ->  tokD cs (TOpen   n [c])
        | isClose   c   ->  tokD cs (TClose  n [c])
        | isSingle  c   ->  tokD cs (TWord n 0 [c])
        | isTerm    c   ->  term cs [c]  []
        | isQuote   c   ->  quote   c cs []
        | isWord    c   ->  word    ccs  []
        | isSpace   c   ->  white 1 cs
      _                 ->  error $ "unknown token type: " ++ ccs
    where
      dropSpaces     = dropWhile isSpace
      tokD cs x = (x, cs)
      tokR cs cons xs = (cons $ reverse xs, cs)

      word (c:cs) xs | isWord c      = word cs (c:xs)
      word ccs2 xs                   = tokR ccs2 (TWord n 0) xs

      quote q [] xs                  = tokR [] (wordQ q) xs
      quote q (c:cs) xs
          | c == q                   = tokR cs (wordQ q) xs
          | otherwise                = quote q cs (c:xs)

      wordQ '\'' = (TWord n 1)
      wordQ '"'  = (TWord n 2)
      wordQ _    = (TWord n 0)

      term (c:cs) xs ns | isTerm c   = term cs [c] $ t xs ns
                        | isWord c   = term cs (c:xs) ns
      term ccs2 xs ns                = tokR ccs2 (TTermN n) (t xs ns)
      t xs = (reverse xs :)

      white i (c:cs)    | isSpace c  = white (i + 1) cs
      white i ccs2                   = tokD ccs2 (TSpace n i)

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
untokens (TOpen _ a : xs)      = a ++ untokens xs
untokens (x : TClose n a : xs) = untoken x ++ untokens (TClose n a : xs)
untokens [x]    = untoken x
untokens (x:xs) = untoken x ++ untokens xs
untokens []     = []

untoken :: Token -> String
untoken (TWord _ 1 s)   = "'" ++ s ++ "'"
untoken (TWord _ 2 s)   = "\"" ++ s ++ "\""
untoken (TWord _ _ s)   = s
untoken (TTermN  _ ns)  = concat ns
untoken (TTermP  _ ns)  = concatMap show ns
untoken (TOpen   _ s)   = s
untoken (TClose  _ s)   = s
untoken (TSpace  _ n)   = replicate n ' '
untoken (TComment _ s)  = s



-- ----------------------
-- $SpecialCharacters



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
-- > ** aaa bbbbb cc
--
-- Clause comments like this:
--
-- > **** aaa bbb
-- >      ccccc dddddd
-- >      ee fffff
--
-- You can type @****@ on top of a clause to hide it.



-- ----------------------
-- $EscapeSequences
--
-- (Not implemented)
--
-- [@\[*q\]@]
--  Single quote.
--
-- [@\[*qq\]@]
--  Double quote.
--
-- [@\[*cr\]@]
--  Carriage return (@\\r@).
--
-- [@\[*lf\]@]
--  Line feed (@\\n@).
--
-- [@\[*tab\]@]
--  Tab (@\\t@).
--
-- [@\[*spc\]@]
--  Space.



-- ----------------------
-- $SyntacticContentType
--
-- (Not implemented)
--
-- Tokenizer recognizes five types of content.
--
-- [Word]
--  Simple sequence of characters,
--  e.g., @abc@, @123@.
--  Numbers are represented as words.
--
-- [Code]
--  Code are like tagged word,
--  e.g., @(color \'blue\')@.
--
-- [List]
--  Orderd sequence of contents.
--  Lists are enclosed in square brackets,
--  e.g., @[ ab cd 0 1 ]@
--
-- [Tuple]
--  Set of pairs of term name and content.
--  Tuples are enclosed in round-bar parens,
--  e.g., @(| \/a 10 \/b 20 |)@.
--
-- [Relation]
--  Set of uniform-typed tuples.
--  Relations are enclosed in square-bar brackets,
--  and enveloped tuples are divided by vertical bar,
--  e.g., @[| \/a \/b | 10 20 | 10 30 |]@.


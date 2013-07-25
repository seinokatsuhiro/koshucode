{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

{-| Tokenizer of koshucode. -}

module Koshucode.Baala.Base.Syntax.Tokenizer
(
  -- * Library

  -- ** Source lines
  SourceLine (..),
  sourceLines,
  divideByToken,

  -- ** Tokenizer
  tokens,
  untokens,
  untoken,

  -- * Document

  -- ** Special characters
  -- $SpecialCharacters

  -- ** Asterisks
  -- $Asterisks

  -- ** Syntactic content type
  -- $SyntacticContentType

  -- * Examples
  -- $Examples
) where

import Data.Generics (Data, Typeable)
import qualified Data.Char as C

import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Base.Syntax.Token



-- ----------------------  Source line

data SourceLine = SourceLine
    { sourceLineNumber  :: Int     -- ^ Line number, from 1.
    , sourceLineContent :: String  -- ^ Line content without newline.
    , sourceLineTokens  :: [Token] -- ^ Tokens in the line.
    } deriving (Show, Eq, Ord, Data, Typeable)

instance Pretty SourceLine where
    doc (SourceLine _ line _) = text line

{-| Split source text into 'SourceLine' list.

    1. Split source code into lines by line delimiters
       (carriage return @\\r@ or line feed @\\n@)

    2. Numbering line numbers.
       Internally, this is represented as
       a list of pairs (/line#/, /string/).

    3. Tokenize each lines.
       This is represented as a list of
       'SourceLine' /line#/ /string/ /tokens/.
  -}
sourceLines
    :: String        -- ^ Source text in koshucode.
    -> [SourceLine]  -- ^ Token list per lines.
sourceLines = loop 1 . linesNumbered where
    loop _ [] = []
    loop tok (p : ps) =
        case sourceLine tok p of
          (tok', src) -> src ++ loop tok' ps

sourceLine :: Int -> (Int, String) -> (Int, [SourceLine])
sourceLine tok (n, ln) = (tok + length toks, map src ls) where
    toks = gatherWith nextToken [tok ..] ln
    ls   = divideByToken "||" toks
    src  = SourceLine n ln

divideByToken :: String -> [Token] -> [[Token]]
divideByToken w = divideByP p where
    p (TWord _ 0 x) | w == x = True
    p _ = False



-- ---------------------- lines

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

{-| Split a next token from source text. -}
nextToken :: Int -> String -> (Token, String)
nextToken n txt =
    case txt of
      ('*' : '*' : '*' : '*' : cs) -> tokD cs (word0 "****")
      ('*' : '*' : _)    ->  tokD "" (TComment n txt)
      ('#' : '!' : _)    ->  tokD "" (TComment n txt)
      ('(' : ')' : cs)   ->  tokD cs (word0 "()")     -- nil

      (c : '|' : cs)
          | c == '{'     ->  tokD cs (open "{|")   -- relation
          | c == '<'     ->  tokD cs (open "<|")   -- tuple
          | c == '['     ->  tokD cs (open "[|")   -- undefined
          | c == '('     ->  tokD cs (open "(|")   -- undefined

      ('|' : c : cs)
          | c == '}'     ->  tokD cs (close "|}")  -- relation
          | c == '>'     ->  tokD cs (close "|>")  -- tuple
          | c == ']'     ->  tokD cs (close "|]")  -- undefined
          | c == ')'     ->  tokD cs (close "|)")  -- undefined
          | c == '|'     ->  tokD (trim cs) (word0 "||") -- newline

      (c : cs)
          | isOpen    c  ->  tokD cs (open  [c])
          | isClose   c  ->  tokD cs (close [c])
          | isSingle  c  ->  tokD cs (word0 [c])
          | isTerm    c  ->  term cs [c]  []
          | isQuote   c  ->  quote   c cs []
          | isWord    c  ->  word    txt  []
          | isSpace   c  ->  white 1 cs
      _                  ->  error $ "unknown token type: " ++ txt

    where
      open  w             =  TOpen  n w
      close w             =  TClose n w
      wordQ '"'           =  TWord  n 2
      wordQ '\''          =  TWord  n 1
      wordQ _             =  TWord  n 0
      word0               =  TWord  n 0

      trim                =  dropWhile isSpace
      tokD cs token       =  (token, cs)
      tokR cs k xs        =  (k $ reverse xs, cs)

      word (c:cs) xs | isWord c   = word cs (c:xs)
      word ccs xs                 = tokR ccs word0 xs

      quote q [] xs               = tokR [] (wordQ q) xs
      quote q (c:cs) xs
          | c == q                = tokR cs (wordQ q) xs
          | otherwise             = quote q cs (c:xs)

      term (c:cs) xs ns 
          | isTerm c              = term cs [c] $ t xs ns
          | isWord c              = term cs (c:xs) ns
      term ccs xs ns              = tokR ccs (TTerm n) (t xs ns)
      t xs = (reverse xs :)

      white i (c:cs) | isSpace c  = white (i + 1) cs
      white i ccs                 = tokD ccs (TSpace n i)



-- ----------------------  Char category

isTerm, isOpen, isClose, isSingle, isQuote :: Char -> Bool
isSpace, isWord, isNonWord, maybeWord :: Char -> Bool

isTerm    c  =  c == '/'
isOpen    c  =  c `elem` "([{"
isClose   c  =  c `elem` "}])"
isSingle  c  =  c `elem` "':"
isQuote   c  =  c `elem` "'\""
isSpace   c  =  C.isSpace c
isWord    c  =  maybeWord c && not (isNonWord c)
isNonWord c  =  isOpen c || isClose c || isSingle c
maybeWord c  =  C.isAlphaNum c
                 || C.isMark c
                 || C.isSymbol c
                 || C.isPunctuation c



-- ----------------------  Untokenizer

{-| Split string into list of tokens.
    Result token list does not contain newline characters. -}
tokens :: String -> [Token]
tokens = concatMap sourceLineTokens . sourceLines

{-| Convert back a token list to a source string. -}
untokens :: [Token] -> String
untokens (TOpen _ a : xs)      = a ++ untokens xs
untokens (x : TClose n a : xs) = untoken x ++ untokens (TClose n a : xs)
untokens [x]    = untoken x
untokens (x:xs) = untoken x ++ untokens xs
untokens []     = []

untoken :: Token -> String
untoken (TWord _ 1 s)   = "'"  ++ s ++ "'"
untoken (TWord _ 2 s)   = "\"" ++ s ++ "\""
untoken (TWord _ _ s)   = s
untoken (TTerm   _ ns)  = concat ns
untoken (TOpen   _ s)   = s
untoken (TClose  _ s)   = s
untoken (TSpace  _ n)   = replicate n ' '
untoken (TComment _ s)  = s



-- ----------------------
-- $SpecialCharacters



-- ----------------------
{- $Asterisks

   There are three uses of asterisks (@*@) in koshucode,
   
   [@*@]     Single asterisk means multiplication,
             e.g., @3 * 4@ (three times four).
   
   [@**@]    Double asterisk leads line comment.
             Textx from @**@ to end of line are ignored.
   
   [@****@]  Quadruple asterisk (fourfold asterisk) leads caluse comment.
             Texts from @****@ to end of clause are ignored.
             In other words, line staring with @****@
             and following indented lines are ignored.
   
   Triple asterisk @***@ is double and rest of text.
   Quintuple (fivefold) asterisk @*****@ is quadruple and rest of text.
   
   Line comments like this:
   
   > ** aaa bbbbb cc
   
   Clause comments like this:
   
   > **** aaa bbb
   >      ccccc dddddd
   >      ee fffff
   
   You can type @****@ on top of a clause to hide it.
   -}



-- ----------------------
{- $SyntacticContentType

   (Not implemented)
   
   Tokenizer recognizes five types of content.
   
   [Word]      Simple sequence of characters,
               e.g., @abc@, @123@.
               Numbers are represented as words.
   
   [Code]      Code are like tagged word,
               e.g., @(color \'blue\')@.
   
   [List]      Orderd sequence of contents.
               Lists are enclosed in square brackets,
               e.g., @[ ab cd 0 1 ]@
   
   [Tuple]     Set of pairs of term name and content.
               Tuples are enclosed in curely braces,
               e.g., @{ \/a 10 \/b 20 }@.
   
   [Relation]  Set of uniform-typed tuples.
               Relations are enclosed in curely-bar braces,
               and enveloped tuples are divided by vertical bar,
               e.g., @{| \/a \/b | 10 20 | 10 30 |}@.

   -}



-- ----------------------
{- $Examples

   Words and quotations.

   >>> tokens "aa\n'bb'\n\"cc\""
   [TWord 0 "aa", TWord 1 "bb", TWord 2 "cc"]
   
   Example includes 'TWord', 'TTerm' and 'TSpace' tokens.
   
   >>> tokens "|-- R  /a A0 /b 31"
   [TWord 0 "|--", TSpace 1, TWord 0 "R", TSpace 2,
    TTerm ["/a"], TSpace 1, TWord 0 "A0", TSpace 1,
    TTerm ["/b"], TSpace 1, TWord 0 "31"]

   Parens.

   >>> tokens "aa (bb x y (z))"
   [TWord 1 0 "aa", TSpace 2 1,
    TOpen 3 "(", TWord 4 0 "bb", TSpace 5 1,
      TWord 6 0 "x", TSpace 7 1, TWord 8 0 "y", TSpace 9 1,
      TOpen 10 "(", TWord 11 0 "z", TClose 12 ")", TClose 13 ")"]

   A comment.

   >>> tokens "abc ** this is a comment\ndef\n"
   [TWord 1 0 "abc", TSpace 2 1, TComment 3 "** this is a comment",
    TWord 4 0 "def"]

-}


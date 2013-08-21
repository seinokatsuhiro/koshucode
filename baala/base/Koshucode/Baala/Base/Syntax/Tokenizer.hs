{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

{-| Tokenizer of koshucode. -}

module Koshucode.Baala.Base.Syntax.Tokenizer
(
  -- * Library
  TokenLine,
  tokenize,
  trimLeft,
  tokens,

  -- * Document

  -- ** Token type
  -- $TokenType

  -- ** Asterisks
  -- $Asterisks

  -- * Examples
  -- $Examples
) where

import qualified Data.Char as C

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax.Token
import Koshucode.Baala.Base.Syntax.CodeLine



-- ----------------------  Tokenizer

{-| Token list on a line. -}
type TokenLine = CodeLine Token

tokenize :: String -> [TokenLine]
tokenize = codeLines nextToken

{-| Split string into list of tokens.
    Result token list does not contain newline characters. -}
tokens :: String -> [Token]
tokens = concatMap codeLineTokens . tokenize

trimLeft :: Map String
trimLeft = dropWhile isSpace

{-| Split a next token from source text. -}
nextToken :: NextToken Token
nextToken n txt =
    case txt of
      ('*' : '*' : '*' : '*' : cs) -> tokD cs (word0 "****")
      ('*' : '*' : _)         ->  tokD "" (TComment n txt)
      ('\'' : '\'' : cs)      ->  tokD "" (TWord n 1 $ trimLeft cs)
      ('(' : ')' : cs)        ->  tokD cs (word0 "()")  -- nil
      ('#' : c : cs)
          | isSpecial c       ->  tokD cs $ word0 [c]
          | c == '!'          ->  tokD "" (TComment n txt)
      (c : '|' : cs)
          | c `elem` "[{<("   ->  tokD cs $ open  [c, '|']
      ('|' : c : cs)
          | c `elem` "]}>)"   ->  tokD cs $ close ['|', c]
          | c == '|'          ->  tokD (trimLeft cs) (word0 "||") -- newline

      (c : cs)
          | isOpen    c       ->  tokD cs $ open  [c]
          | isClose   c       ->  tokD cs $ close [c]
          | isSingle  c       ->  tokD cs $ word0 [c]
          | isTerm    c       ->  term cs [c]  []
          | isQuote   c       ->  quote   c cs []
          | isWord    c       ->  word    cs   [c]
          | isSpace   c       ->  white 1 cs

      _ -> tokD [] $ TUnknown n []

    where
      open  w                     =  TOpen  n w
      close w                     =  TClose n w
      wordQ '"'                   =  TWord  n 2
      wordQ _                     =  TWord  n 0
      word0                       =  TWord  n 0

      tokD cs token               =  (token, cs)
      tokR cs k xs                =  (k $ reverse xs, cs)

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

isTerm, isQuote, isOpen, isClose             :: Char -> Bool
isSingle, isSpecial, isOCS, isSpace, isWord  :: Char -> Bool

isTerm    c  =  c == '/'          -- UnicodePunctuation
isQuote   c  =  c == '\"'         -- UnicodePunctuation
isOpen    c  =  c `elem` "([{"    -- UnicodePunctuation
isClose   c  =  c `elem` "}])"    -- UnicodePunctuation
isSingle  c  =  c `elem` "':"     -- UnicodePunctuation
isOCS     c  =  isOpen c || isClose c || isSingle c
isSpecial c  =  isOCS c || isTerm c
isSpace   c  =  C.isSpace c       -- UnicodeSeprator | UnicodeOther
isWord    c  =  isw $ generalCategoryGroup c where
    isw UnicodeLetter      =  True
    isw UnicodeMark        =  True
    isw UnicodeNumber      =  True
    isw UnicodePunctuation =  not $ isOCS c
    isw UnicodeSymbol      =  True
    isw UnicodeSeperator   =  False
    isw UnicodeOther       =  False



-- ----------------------
{- $TokenType

   [Paren]     Open and closed parens.
               @(@ /group/ @)@ ,
               @{@ /set/ @}@ ,
               @[@ /list/ @]@ ,
               @\<|@ /termset/ @\|>@ , and
               @{|@ /relation/ @|}@

   [Termname]  Words beginning with slash, e.g., @\/aa@.
               Term name like @\/r\/x@ is used for nested relation,
               that means term @\/x@ in the relation of term @\/r@.

   [Word]      Character sequence not including special characters,
               e.g., @aa@, @r2d2@, @12.0@, etc.
               Colon @:@ is a one-letter word.
               There are four types of quotations.
               (1) non-quoted word like @aa@,
               (2) single-quoted word like @\'aa@,
               (3) double-quoted word like @\"aa\"@,
               (4) doubly single-quoted word like @\'\' all-letters-to-end-of-line@.

   [Comment]   Texts from double asterisks (@**@) or shebang (@#!@)
               to end of line are comments.
               Quadruple asterisks (@****@) comments are
               treated in 'Koshucode.Baala.Core.Section.Clause.Clause'.

   [Space]     /space/ , @\\t@ (tab)

   [Unknown]   Other than those above.

-}



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
{- $Examples

   Words and quotations.

   >>> tokens "aa\n'bb'\n\"cc\""
   [TWord 1 0 "aa", TWord 2 0 "'", TWord 3 0 "bb",
    TWord 4 0 "'", TWord 5 2 "cc"]
   
   Example includes 'TWord', 'TTerm' and 'TSpace' tokens.
   
   >>> tokens "|-- R  /a A0 /b 31"
   [TWord 1 0 "|--", TSpace 2 1, TWord 3 0 "R",
    TSpace 4 2, TTerm 5 ["/a"], TSpace 6 1, TWord 7 0 "A0",
    TSpace 8 1, TTerm 9 ["/b"], TSpace 10 1, TWord 11 0 "31"]

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


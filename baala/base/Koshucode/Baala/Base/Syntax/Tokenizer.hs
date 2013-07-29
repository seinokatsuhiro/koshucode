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
  tokens,

  -- * Document

  -- ** Token category
  -- $TokenCategory

  -- ** Asterisks
  -- $Asterisks

  -- * Examples
  -- $Examples
) where

import Data.Generics (Data, Typeable)
import qualified Data.Char as C

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax.Token



-- ----------------------  Source line

data SourceLine = SourceLine
    { sourceLineNumber  :: LineNumber -- ^ Line number, from 1.
    , sourceLineContent :: String     -- ^ Line content without newline.
    , sourceLineTokens  :: [Token]    -- ^ Tokens in the line.
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
sourceLines = sourceLinesBy sourceLine

sourceLinesBy
    :: (TNumber -> (LineNumber, String) -> (TNumber, [SourceLine]))
    -> String -> [SourceLine]
sourceLinesBy f = loop 1 . linesCrlfNumbered where
    loop _ [] = []
    loop tok (p : ps) =
        case f tok p of
          (tok', src) -> src ++ loop tok' ps

sourceLine
    :: TNumber                  -- ^ Token number
    -> (LineNumber, String)     -- ^ Line number and content
    -> (TNumber, [SourceLine])  -- ^ Token number and 'SourceLine'
sourceLine tok (n, ln) = (tok + length toks, map src ls) where
    toks = gatherWith nextToken [tok ..] ln
    ls   = divideByToken "||" toks
    src  = SourceLine n ln

{-| Divide token list by some word. -}
divideByToken :: String -> [Token] -> [[Token]]
divideByToken w = divideByP p where
    p (TWord _ 0 x) | w == x = True
    p _ = False

{-| Split string into list of tokens.
    Result token list does not contain newline characters. -}
tokens :: String -> [Token]
tokens = concatMap sourceLineTokens . sourceLines



-- ----------------------  Tokenizer

{-| Split a next token from source text. -}
nextToken :: TNumber -> String -> (Token, String)
nextToken n txt =
    case txt of
      ('*' : '*' : '*' : '*' : cs) -> tokD cs (word0 "****")
      ('*' : '*' : _)         ->  tokD "" (TComment n txt)
      ('\'' : '\'' : cs)      ->  tokD "" (TWord n 1 $ trim cs)
      ('(' : ')' : cs)        ->  tokD cs (word0 "()")  -- nil
      ('#' : c : cs)
          | isSpecial c       ->  tokD cs $ word0 [c]
          | c == '!'          ->  tokD "" (TComment n txt)
      (c : '|' : cs)
          | c `elem` "[{<("   ->  tokD cs $ open  [c, '|']
      ('|' : c : cs)
          | c `elem` "]}>)"   ->  tokD cs $ close ['|', c]
          | c == '|'          ->  tokD (trim cs) (word0 "||") -- newline

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

      trim                        =  dropWhile isSpace
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
{- $TokenCategory

   [Paren]            @(@ group @)@ ,
                      @{@ set @}@ ,
                      @[@ list @]@ ,
                      @\<|@ termset @\|>@ ,
                      @{|@ relation @|}@

   [Term name]        @\/@xxx , @\/@rel@\/@xxx

   [Word]             @abc@ , @123@ , @|@ ,
                      @\#(@ , @\#)@ , @****@ ,
                      @''@ all-letters-to-end-of-line

   [One-letter word]  @:@ , @'@

   [Comment]          @**@ all-letters-to-end-of-line ,
                      @#!@ all-letters-to-end-of-line

   [Space]            /space/ , @\\t@ (tab)

   [Newline]          @\\r@ (carriage return) ,
                      @\\n@ (line feed) ,
                      @||@ (pseudo newline)

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


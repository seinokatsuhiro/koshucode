{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokenizer of koshucode.

module Koshucode.Baala.Base.Token.TokenLine
(
  -- * Library
  TokenLine,
  tokenLines,
  tokens,
  isSimpleWord, isSimpleChar,

  -- * Document

  -- ** Token type
  -- $TokenType

  -- ** Asterisks
  -- $Asterisks

  -- * Examples
  -- $Examples
) where

import qualified Data.Char                           as Ch
import qualified Koshucode.Baala.Base.Prelude        as B
import qualified Koshucode.Baala.Base.Syntax         as B
import qualified Koshucode.Baala.Base.Text           as B
import qualified Koshucode.Baala.Base.Token.Token    as B
import qualified Koshucode.Baala.Base.Token.HashWord as B



-- ----------------------  Tokenizer

-- | Token list on a line.
type TokenLine = B.CodeLine B.Token

-- | Tokenize text.
tokenLines :: B.Resource -> String -> [TokenLine]
tokenLines res = B.codeLines $ nextToken res

-- | Split string into list of tokens.
--   Result token list does not contain newline characters.
tokens :: B.Resource -> String -> [B.Token]
tokens res = concatMap B.lineTokens . tokenLines res

-- | Split a next token from source text.
nextToken :: B.Resource -> B.NextToken B.Token
nextToken res (num, line) txt =
    case txt of
      ('*' : '*' : '*' : '*' : cs)
                             ->  tokD cs          $ B.TWord     p 0 "****"
      ('*' : '*' : _)        ->  tokD ""          $ B.TComment  p txt
      ('\'' : '\'' : cs)     ->  let cs2 = B.trimLeft cs
                                 in tokD ""       $ B.TWord     p 1 cs2
      ('(' : ')' : cs)       ->  tokD cs          $ B.TWord     p 0 "()"  -- nil
      ('#' : c : cs)
          | c == '!'         ->  tokD ""          $ B.TComment  p txt
          | otherwise        ->  word cs [c]      hash
                                   
      (c : '|' : cs)
          | c `elem` "[{<("  ->  tokD cs          $ B.TOpen     p [c, '|']
      ('|' : c : cs)
          | c `elem` "]}>)"  ->  tokD cs          $ B.TClose    p ['|', c]
          | c == '|'         ->  let cs2 = B.trimLeft cs  -- newline
                                 in tokD cs2      $ B.TWord     p 0 "||"

      (c : cs)
          | isOpen    c      ->  tokD cs          $ B.TOpen     p [c]
          | isClose   c      ->  tokD cs          $ B.TClose    p [c]
          | isSingle  c      ->  tokD cs          $ B.TWord     p 0 [c]
          | isTerm    c      ->  term cs [] []
          | isQQ      c      ->  qq   cs
          | isQ       c      ->  word cs []       $ B.TWord     p 1
          | isSlot    c      ->  let (n, cs2) = slot 1 cs
                                 in word cs2 []   $ B.TSlot     p n
          | isShort   c      ->  short cs [c]
          | isWord    c      ->  word cs [c]      $ B.TWord     p 0
          | isSpace   c      ->  space 1 cs

      _                      ->  tokD []          $ B.TUnknown  p []

    where
      p = B.CodePoint res num line txt

      hash s = case lookup s B.hashWordTable of
                 Nothing -> B.TWord p 0 $ '#' : s
                 Just t  -> B.TWord p 3 t

      tokD :: String -> B.Token -> (B.Token, String)
      tokD cs token                   = (token, cs)

      tokR :: String -> ([a] -> B.Token) -> [a] -> (B.Token, String)
      tokR cs k xs                    = (k $ reverse xs, cs)

      short :: String -> String -> (B.Token, String)
      short (c:cs) xs | c == '.'      = word cs [] (B.TShort p $ reverse xs)
                      | isShort c     = short cs (c:xs)
      short ccs xs                    = word ccs xs (B.TWord p 0)

      slot :: Int -> String -> (Int, String)
      slot n ('@'  : cs) = slot (n + 1) cs
      slot _ ('\'' : cs) = (0, cs)
      slot n cs          = (n, cs)

      word :: String -> String -> (String -> B.Token) -> (B.Token, String)
      word (c:cs) text k | isWord c   = word cs (c : text) k
      word ccs    text k              = tokR ccs k text

      qqText :: String -> String -> Maybe (String, String)
      qqText [] _                     = Nothing
      qqText (c:cs) text | isQQ c     = Just (text, cs)
                         | otherwise  = qqText cs (c : text)

      qq :: String -> (B.Token, String)
      qq cs                           = case qqText cs [] of
                                          Just (text, cs2) -> tokR cs2 (B.TWord p 2) text
                                          Nothing -> tokD [] $ B.TUnknown p cs

      term :: String -> String -> [String] -> (B.Token, String)
      term (c:cs) xs ns | isTerm c    = term cs [] $ termUp xs ns
                        | isWord c    = term cs (c:xs) ns
                        | isQQ   c    = case qqText cs xs of
                                          Just (text, cs2) -> term cs2 [] $ termUp text ns
                                          Nothing -> tokD [] $ B.TUnknown p (c:cs)
      term ccs xs ns                  = tokR ccs (B.TTerm p) $ termUp xs ns

      termUp :: String -> [String] -> [String]
      termUp [] ns = ns 
      termUp xs ns = reverse xs : ns

      space :: Int -> String -> (B.Token, String)
      space i (c:cs) | isSpace c      = space (i + 1) cs
      space i ccs                     = tokD ccs $ B.TSpace p i



-- ----------------------  Char category

--  ( ) [ ] { } | : " ' # /
--  O C O C O C S S Q Q H T

-- Punctuations
isOpen, isClose, isSingle, isQ, isQQ, isSlot, isPunct :: B.Pred Char

isOpen    =  (`elem` "([{")   -- UnicodePunctuation
isClose   =  (`elem` "}])")   -- UnicodePunctuation
isSingle  =  (`elem` ":|")    -- UnicodePunctuation | UnicodeSymbol
isQ       =  (== '\'')        -- UnicodePunctuation
isQQ      =  (== '"')         -- UnicodePunctuation
isSlot    =  (== '@')         -- UnicodePunctuation
isPunct c =  isOpen c || isClose c || isSingle c ||
             isQ c    || isQQ c    || c == '#'

isTerm, isSpace, isWord  :: B.Pred Char

isTerm     =  (== '/')        -- UnicodePunctuation
isSpace c  =  Ch.isSpace c    -- UnicodeSeprator | UnicodeOther
isWord  c  =  test $ B.generalCategoryGroup c where
    test B.UnicodeLetter      = True
    test B.UnicodeNumber      = True
    test B.UnicodeMark        = True
    test B.UnicodeSymbol      = not $ c == '|'
    test B.UnicodePunctuation = not $ isPunct c
    test B.UnicodeSeperator   = False
    test B.UnicodeOther       = False

isSimpleWord :: B.Pred String
isSimpleWord = all isSimpleChar

isSimpleChar :: B.Pred Char
isSimpleChar c =
    case B.generalCategoryGroup c of
      B.UnicodeLetter      -> True
      B.UnicodeNumber      -> True
      B.UnicodeSymbol      -> c `elem` "+<=>"
      B.UnicodePunctuation -> c `elem` "-_.,!?"
      _                    -> False

isShort :: Char -> Bool
isShort = Ch.isAlpha


-- ------------------------------------------------------------------
-- $TokenType
--
--  [Paren]     Open and closed parens.
--              @(@ /group/ @)@ ,
--              @{@ /set/ @}@ ,
--              @[@ /list/ @]@ ,
--              @\<|@ /termset/ @\|>@ , and
--              @{|@ /relation/ @|}@
--
--  [TermName]  Words beginning with slash, e.g., @\/aa@.
--              Term name like @\/r\/x@ is used for nested relation,
--              that means term @\/x@ in the relation of term @\/r@.
--
--  [Word]      Character sequence not including special characters,
--              e.g., @aa@, @r2d2@, @12.0@, etc.
--              Colon @:@ is a one-letter word.
--              There are four types of quotations.
--              (1) non-quoted word like @aa@,
--              (2) single-quoted word like @\'aa@,
--              (3) double-quoted word like @\"aa\"@,
--              (4) doubly single-quoted word like @\'\' all-letters-to-end-of-line@.
--
--  [Comment]   Texts from double asterisks (@**@) or shebang (@#!@)
--              to end of line are comments.
--              Quadruple asterisks (@****@) comments are
--              treated in 'Koshucode.Baala.Core.Section.Clause.Clause'.
--
--  [Space]     /space/ , @\\t@ (tab)
--
--  [Unknown]   Other than those above.
--

-- ------------------------------------------------------------------
-- $Asterisks
--
--  There are three uses of asterisks (@*@) in koshucode,
--
--  [@*@]     Single asterisk means multiplication,
--            e.g., @3 * 4@ (three times four).
--
--  [@**@]    Double asterisk leads line comment.
--            Textx from @**@ to end of line are ignored.
--
--  [@****@]  Quadruple asterisk (fourfold asterisk) leads caluse comment.
--            Texts from @****@ to end of clause are ignored.
--            In other words, line staring with @****@
--            and following indented lines are ignored.
--
--  Triple asterisk @***@ is double and rest of text.
--  Quintuple (fivefold) asterisk @*****@ is quadruple and rest of text.
--
--  Line comments like this:
--
--  > ** aaa bbbbb cc
--
--  Clause comments like this:
--
--  > **** aaa bbb
--  >      ccccc dddddd
--  >      ee fffff
--
--  You can type @****@ on top of a clause to hide it.
--

-- ------------------------------------------------------------------
-- $Examples
--
--  Words and quotations.
--
--  >>> tokens "aa\n'bb'\n\"cc\""
--  [ TWord 1 0 "aa", TWord 2 1 "bb", TWord 3 1 "", TWord 4 2 "cc" ]
--
--  Judgement.
--
--  >>> tokens "|-- R  /a A0 /b 31"
--  [ TWord 1 0 "|", TWord 2 0 "--", TSpace 3 1, TWord 4 0 "R"
--  , TSpace 5 2, TTerm 6 ["/a"], TSpace 7 1, TWord 8 0 "A0"
--  , TSpace 9 1, TTerm 10 ["/b"], TSpace 11 1, TWord 12 0 "31" ]
--
--  Parens.
--
--  >>> tokens "aa (bb x y (z))"
--  [ TWord 1 0 "aa", TSpace 2 1
--  , TOpen 3 "(", TWord 4 0 "bb", TSpace 5 1
--     , TWord 6 0 "x", TSpace 7 1, TWord 8 0 "y", TSpace 9 1
--     , TOpen 10 "(", TWord 11 0 "z", TClose 12 ")", TClose 13 ")" ]
--
--  A comment.
--
--  >>> tokens "abc ** this is a comment\ndef\n"
--  [ TWord 1 0 "abc", TSpace 2 1, TComment 3 "** this is a comment"
--  , TWord 4 0 "def"]
--

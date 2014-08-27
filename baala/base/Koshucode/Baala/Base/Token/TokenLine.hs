{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokenizer of koshucode.

module Koshucode.Baala.Base.Token.TokenLine
(
  -- * Library
  TokenLine,
  tokenLines,
  tokens,
  isShortPrefix,

  -- * Document

  -- ** Token type
  -- $TokenType

  -- ** Asterisks
  -- $Asterisks

  -- * Examples
  -- $Examples
) where

import qualified Data.Char                           as Char
import qualified Koshucode.Baala.Base.Prelude        as B
import qualified Koshucode.Baala.Base.Syntax         as B
import qualified Koshucode.Baala.Base.Text           as B
import qualified Koshucode.Baala.Base.Token.Token    as B
import qualified Koshucode.Baala.Base.Token.Bracket  as B



-- ----------------------  Tokenizer

-- | Token list on a line.
type TokenLine = B.CodeLine B.Token

-- | Split string into list of tokens.
--   Result token list does not contain newline characters.
tokens :: B.Resource -> String -> [B.Token]
tokens res = concatMap B.lineTokens . tokenLines res

-- | Tokenize text.
tokenLines :: B.Resource -> String -> [TokenLine]
tokenLines res = B.codeLines $ nextToken res

-- | Split a next token from source text.
nextToken :: B.Resource -> B.NextToken B.Token
nextToken res (num, line) txt =
    case txt of
      '*' : '*' : '*' : '*' : cs
                      ->  token cs        $ B.TText     p 0 "****"
      '*' : '*' : _   ->  token ""        $ B.TComment  p txt
      '(' : ')' : cs  ->  token cs        $ B.TText     p 0 "()"  -- empty
      '<' : '<' : cs  ->  token cs        $ B.TOpen     p "<<"
      '>' : '>' : cs  ->  token cs        $ B.TClose    p ">>"
                              
      c : '|' : cs
          | isOpen c  ->  token cs        $ B.TOpen     p [c, '|']
      '|' : c : cs
          | isClose c ->  token cs        $ B.TClose    p ['|', c]
          | c == '|'  ->  let cs2         = B.trimLeft cs  -- newline
                          in token cs2    $ B.TText     p 0 "||"

      '<' : cs        ->  angle cs []
      '#' : '!' : _   ->  token ""        $ B.TComment  p txt
      '@' : cs        ->  let (n, cs2)    = slot 1 cs
                          in word cs2 []  $ B.TSlot     p n
      c : cs
        | isOpen   c  ->  token cs        $ B.TOpen     p   [c]
        | isClose  c  ->  token cs        $ B.TClose    p   [c]
        | isSingle c  ->  token cs        $ B.TText     p 0 [c]
        | isTerm   c  ->  term  cs [] []
        | isQQ     c  ->  qq    cs
        | isQ      c  ->  word  cs []     $ B.TText     p 1
        | isShort  c  ->  short cs [c]
        | isWord   c  ->  word  cs [c]    $ B.TText     p 0
        | isSpace  c  ->  space 1 cs

      _               ->  token []        $ B.TUnknown  p []

    where
      p = B.CodePt res num line txt

      token :: String -> B.Token -> (B.Token, String)
      token cs tok                    =  (tok, cs)

      tokenFrom :: String -> [a] -> ([a] -> B.Token) -> (B.Token, String)
      tokenFrom cs xs k               =  (k $ reverse xs, cs)

      short :: String -> String -> (B.Token, String)
      short (c:cs) pre | c == '.'     =  word  cs []  $ B.TShort p (reverse pre)
                       | isShort c    =  short cs (c : pre)
      short cs     pre                =  word  cs pre $ B.TText p 0

      slot :: Int -> String -> (Int, String)
      slot n ('@'  : cs)              =  slot (n + 1) cs
      slot _ ('\'' : cs)              =  (0, cs) -- positional slots
      slot n cs                       =  (n, cs)

      word :: String -> String -> (String -> B.Token) -> (B.Token, String)
      word cs@('>' : '>' : _) text k  =  tokenFrom cs text  k
      word (c:cs) text k | isWord c   =  word cs (c : text) k
      word cs     ""   k              =  tokenFrom cs "'"   k
      word cs     text k              =  tokenFrom cs text  k

      qq :: String -> (B.Token, String)
      qq cs = case qqText cs [] of
                Just (text, cs2) -> tokenFrom cs2 text $ B.TText p 2
                Nothing -> token [] $ B.TUnknown p cs

      qqText :: String -> String -> Maybe (String, String)
      qqText []     _                 =  Nothing
      qqText (c:cs) text | isQQ c     =  Just (text, cs)
                         | otherwise  =  qqText cs (c : text)

      angle (c:cs) text | c == '>'    =  angleToken cs $ reverse text
                        | isWord c    =  angle cs (c : text)
      angle cs     text               =  token cs $ B.TText p 0 ('<' : reverse text)

      angleToken cs ('c' : code)
          | all isCode code           =  case mapM B.readInt $ B.omit null $ B.divide '-' code of
                                           Just ns  ->  token cs $ B.TText p 3 $ map Char.chr ns
                                           Nothing  ->  token cs $ B.TText p (-1) code
      angleToken cs text | null text  =  token cs $ B.TText p 0 "<>"
                         | otherwise  =  case lookup text B.bracketKeywords of
                                           Just w   ->  token cs $ B.TText p 3 w
                                           Nothing  ->  token cs $ B.TText p (-1) text

      isCode :: Char -> Bool
      isCode '-' = True
      isCode c   = Char.isDigit c

      term :: String -> String -> [String] -> (B.Token, String)
      term (c:cs) xs ns | isTerm c    =  term cs [] $ termUp xs ns
                        | isWord c    =  term cs (c:xs) ns
                        | isQQ   c    =  case qqText cs xs of
                                           Just (text, cs2) -> term cs2 [] $ termUp text ns
                                           Nothing          -> token [] $ B.TUnknown p (c:cs)
      term cs     xs ns               =  tokenFrom cs (termUp xs ns) $ B.TTerm p 0

      termUp :: String -> [String] -> [String]
      termUp [] ns                    =  ns 
      termUp xs ns                    =  reverse xs : ns

      space :: Int -> String -> (B.Token, String)
      space i (c:cs) | isSpace c      =  space (i + 1) cs
      space i cs                      =  token cs $ B.TSpace p i



-- ----------------------  Char category

--  ( ) [ ] { } | : " ' # /
--  O C O C O C S S Q Q H T

-- Punctuations
isOpen, isClose, isSingle, isQ, isQQ, isPunct :: B.Pred Char

isOpen     =  ( `elem` "([{" )  --  UnicodePunctuation
isClose    =  ( `elem` "}])" )  --  UnicodePunctuation
isSingle   =  ( `elem` ":|"  )  --  UnicodePunctuation | UnicodeSymbol
isQ        =  (    ==  '\''  )  --  UnicodePunctuation
isQQ       =  (    ==  '"'   )  --  UnicodePunctuation
isPunct c  =  isOpen c || isClose c || isSingle c ||
              isQ c    || isQQ c    || c == '#'   || c == '/'

isTerm, isSpace, isWord  :: B.Pred Char

isTerm     =  ( == '/' )      --  UnicodePunctuation
isSpace    =  Char.isSpace    --  UnicodeSeprator | UnicodeOther
isWord  c  =  case B.generalCategoryGroup c of
                B.UnicodeLetter       ->  True
                B.UnicodeNumber       ->  True
                B.UnicodeMark         ->  True
                B.UnicodeSymbol       ->  c /= '|'
                B.UnicodePunctuation  ->  not $ isPunct c
                B.UnicodeSeperator    ->  False
                B.UnicodeOther        ->  False

isShortPrefix :: String -> Bool
isShortPrefix = all isShort

isShort :: Char -> Bool
isShort = Char.isAlpha


-- ------------------------------------------------------------------
-- $TokenType
--
--  [Bracket]   Open and closed brackets.
--              @(@ /group/ @)@ ,
--              @{@ /set/ @}@ ,
--              @[@ /list/ @]@ ,
--              @\<\<@ /assn/ @\>\>@ , and
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
--  [ TText 1 0 "aa", TText 2 1 "bb", TText 3 1 "", TText 4 2 "cc" ]
--
--  Judgement.
--
--  >>> tokens "|-- R  /a A0 /b 31"
--  [ TText 1 0 "|", TText 2 0 "--", TSpace 3 1, TText 4 0 "R"
--  , TSpace 5 2, TTerm 6 ["/a"], TSpace 7 1, TText 8 0 "A0"
--  , TSpace 9 1, TTerm 10 ["/b"], TSpace 11 1, TText 12 0 "31" ]
--
--  Brackets.
--
--  >>> tokens "aa (bb x y (z))"
--  [ TText 1 0 "aa", TSpace 2 1
--  , TOpen 3 "(", TText 4 0 "bb", TSpace 5 1
--     , TText 6 0 "x", TSpace 7 1, TText 8 0 "y", TSpace 9 1
--     , TOpen 10 "(", TText 11 0 "z", TClose 12 ")", TClose 13 ")" ]
--
--  A comment.
--
--  >>> tokens "abc ** this is a comment\ndef\n"
--  [ TText 1 0 "abc", TSpace 2 1, TComment 3 "** this is a comment"
--  , TText 4 0 "def"]
--

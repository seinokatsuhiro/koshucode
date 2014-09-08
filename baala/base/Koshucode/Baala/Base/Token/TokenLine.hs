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
tokenLines = B.codeLines2 tokenize

-- tokenLines :: B.Resource -> String -> [TokenLine]
-- tokenLines = B.codeLines . nextToken

-- | Split a next token from source text.
nextToken :: B.Resource -> B.NextToken B.Token
nextToken res (num, line) txt =
    case txt of
      '*' : '*' : '*' : '*' : cs
                            ->  token cs  $ B.TText     cp 0 "****"
      '*' : '*' : _         ->  token ""  $ B.TComment  cp txt
      '<' : '<' : '<' : cs  ->  token cs  $ B.TOpen     cp "<<<"
      '>' : '>' : '>' : cs  ->  token cs  $ B.TClose    cp ">>>"
      '<' : '<' : cs        ->  token cs  $ B.TOpen     cp "<<"
      '>' : '>' : cs        ->  token cs  $ B.TClose    cp ">>"
                              
      c : '|' : cs
          | isOpen c  ->  token cs        $ B.TOpen     cp [c, '|']
      '|' : c : cs
          | isClose c ->  token cs        $ B.TClose    cp ['|', c]
          | c == '|'  ->  let cs2         = B.trimLeft cs  -- newline
                          in token cs2    $ B.TText     cp 0 "||"

      '<' : cs        ->  angle cs []
      '#' : '!' : _   ->  token ""        $ B.TComment  cp txt
      '@' : cs        ->  let (n, cs2)    = slot 1 cs
                          in word cs2 []  $ B.TSlot     cp n
      c : cs
        | isOpen   c  ->  token cs        $ B.TOpen     cp   [c]
        | isClose  c  ->  token cs        $ B.TClose    cp   [c]
        | isSingle c  ->  token cs        $ B.TText     cp 0 [c]
        | isTerm   c  ->  term  cs [] []
        | isQQ     c  ->  qq    cs
        | isQ      c  ->  word  cs []     $ B.TText     cp 1
        | isShort  c  ->  short cs [c]
        | isWord   c  ->  word  cs [c]    $ B.TText     cp 0
        | isSpace  c  ->  space 1 cs

      _               ->  token []        $ B.TUnknown  cp []

    where
      cp = B.CodePt res num line txt

      token :: String -> B.Token -> (B.Token, String)
      token cs tok                    =  (tok, cs)

      tokenFrom :: String -> [a] -> ([a] -> B.Token) -> (B.Token, String)
      tokenFrom cs xs k               =  (k $ reverse xs, cs)

      short :: String -> String -> (B.Token, String)
      short (c:cs) pre | c == '.'     =  word  cs []  $ B.TShort cp (reverse pre)
                       | isShort c    =  short cs (c : pre)
      short cs     pre                =  word  cs pre $ B.TText cp 0

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
                Just (text, cs2) -> tokenFrom cs2 text $ B.TText cp 2
                Nothing -> token [] $ B.TUnknown cp cs

      qqText :: String -> String -> Maybe (String, String)
      qqText []     _                 =  Nothing
      qqText (c:cs) text | isQQ c     =  Just (text, cs)
                         | otherwise  =  qqText cs (c : text)

      angle (c:cs) text | c == '>'    =  angleToken cs $ reverse text
                        | isWord c    =  angle cs (c : text)
      angle cs     text               =  token cs $ B.TText cp 0 ('<' : reverse text)

      angleToken cs ('c' : code)
          | all isCode code           =  case mapM B.readInt $ B.omit null $ B.divide '-' code of
                                           Just ns  ->  token cs $ B.TText cp 3 $ map Char.chr ns
                                           Nothing  ->  token cs $ B.TText cp (-1) code
      angleToken cs text | null text  =  token cs $ B.TText cp 0 "<>"
                         | otherwise  =  case lookup text B.bracketKeywords of
                                           Just w   ->  token cs $ B.TText cp 3 w
                                           Nothing  ->  token cs $ B.TText cp (-1) text

      term :: String -> String -> [String] -> (B.Token, String)
      term (c:cs) xs ns | isTerm c    =  term cs [] $ termUp xs ns
                        | isWord c    =  term cs (c:xs) ns
                        | isQQ   c    =  case qqText cs xs of
                                           Just (text, cs2) -> term cs2 [] $ termUp text ns
                                           Nothing          -> token [] $ B.TUnknown cp (c:cs)
      term cs     xs ns               =  tokenFrom cs (termUp xs ns) $ B.TTerm cp 0

      termUp :: String -> [String] -> [String]
      termUp [] ns                    =  ns 
      termUp xs ns                    =  reverse xs : ns

      space :: Int -> String -> (B.Token, String)
      space i (c:cs) | isSpace c      =  space (i + 1) cs
      space i cs                      =  token cs $ B.TSpace cp i

tokenize :: B.Tokenize B.Token
tokenize res = call gen where

    call f cs             =  f cs $ res { B.codePtText = cs }
    next f cs tok         =  tok : call f cs
    stop tok              =  [tok]
    rv = reverse

    gen [] _ = []
    gen (c:'|':cs) cp
        | isOpen c        =  next gen cs        $ B.TOpen    cp [c, '|']
    gen (c:cs) cp
        | c == '*'        =  ast  cs cp [c]
        | c == '<'        =  bra  cs cp [c]
        | c == '>'        =  cket cs cp [c]
        | c == '@'        =  slot cs cp [c]
        | c == '|'        =  bar  cs cp [c]
        | c == '#'        =  hash cs cp [c]
        | isOpen c        =  next gen cs        $ B.TOpen    cp [c]
        | isClose c       =  next gen cs        $ B.TClose   cp [c]
        | isSingle c      =  next gen cs        $ B.TText    cp 0 [c]
        | isTerm c        =  term  cs cp [] ""
        | isQQ c          =  qq    cs cp ""
        | isQ c           =  q     cs cp ""
        | isShort c       =  short cs cp [c]
        | isWord c        =  word  cs cp [c]
        | isSpace c       =  space cs cp 1
        | otherwise       =  next gen cs        $ B.TUnknown cp [c]

    ast (c:cs) cp w
        | w == "****"     =  next gen cs        $ B.TText    cp 0 w
        | c == '*'        =  ast cs cp $ c:w
    ast cs cp w
        | w == "*"        =  next gen cs        $ B.TText    cp 0 w
        | w == "**"       =  stop               $ B.TComment cp cs
        | w == "***"      =  stop               $ B.TComment cp cs
        | otherwise       =  next gen cs        $ B.TUnknown cp w

    bra (c:cs) cp w
        | c == '<'        =  bra cs cp $ c:w
    bra cs cp w
        | w == "<"        =  angle cs cp ""
        | w == "<<"       =  next gen cs        $ B.TOpen    cp w
        | w == "<<<"      =  next gen cs        $ B.TOpen    cp w
        | otherwise       =  next gen cs        $ B.TUnknown cp w

    cket (c:cs) cp w
        | c == '>'        =  cket cs cp $ c:w
    cket cs cp w
        | w == ">"        =  word cs cp w
        | w == ">>"       =  next gen cs        $ B.TClose   cp w
        | w == ">>>"      =  next gen cs        $ B.TClose   cp w
        | otherwise       =  next gen cs        $ B.TUnknown cp w

    slot (c:cs) cp w
        | c == '@'        =  slot cs cp $ c:w
        | c == '\''       =  slotName cs     cp 0 ""
        | w == "@"        =  slotName (c:cs) cp 1 ""
        | w == "@@"       =  slotName (c:cs) cp 2 ""
    slot cs cp w          =  next gen cs        $ B.TUnknown cp w

    slotName (c:cs) cp n w
        | isWord c        =  slotName cs cp n $ c:w
    slotName cs cp n w    =  next gen cs        $ B.TSlot cp n $ rv w

    hash ('!':cs) cp _    =  stop               $ B.TComment cp cs
    hash cs cp w          =  next gen cs        $ B.TText cp 0 w

    term (c:cs) cp ns w
        | isTerm c        =  term cs cp (rv w : ns) ""
        | isWord c        =  term cs cp ns $ c:w
    term cs cp ns w       =  next gen cs        $ B.TTerm cp 0 $ rv (rv w : ns)

    qq (c:cs) cp w
        | isQQ c          =  next gen cs        $ B.TText cp 2 $ rv w
        | otherwise       =  qq cs cp $ c:w
    qq _ cp w             =  stop               $ B.TUnknown cp w

    q (c:cs) cp w
        | isWord c        =  q cs cp $ c:w
    q cs cp w             =  next gen cs        $ B.TText cp 1 $ rv w

    short (c:cs) cp w
        | c == '.'        =  shortBody cs cp (rv w) ""
        | isWord c        =  short cs cp $ c:w
    short cs cp w         =  next gen cs        $ B.TText cp 0 $ rv w

    shortBody (c:cs) cp pre w
        | isWord c        =  shortBody cs cp pre $ c:w
    shortBody cs cp pre w = next gen cs         $ B.TShort cp pre $ rv w

    word (c:cs) cp w
        | isWord c        =  word cs cp $ c:w
    word cs cp w          =  next gen cs        $ B.TText cp 0 $ rv w

    space (c:cs) cp n
        | isSpace c       =  space cs cp $ n + 1
    space cs cp n         =  next gen cs        $ B.TSpace cp n

    bar [] cp w           =  stop               $ B.TText cp 0 w
    bar (c:cs) cp w
        | c == '|'        =  bar cs cp $ c:w
        | w == "||"       =  let cs' = B.trimLeft (c:cs)
                            in next gen cs'     $ B.TText cp 0 w
        | w == "|" &&
          isClose c       =  next gen cs        $ B.TClose cp ['|', c]
        | w == "|"        =  next gen (c:cs)    $ B.TText cp 0 w
        | otherwise       =  next gen (c:cs)    $ B.TUnknown cp w

    angle [] cp w        =  stop               $ B.TText cp 0 $ '<' : rv w
    angle (c:cs) cp w
        | c == '>'       =  angleToken cs cp $ rv w
        | isWord c       =  angle cs cp $ c:w
        | otherwise      =  next gen (c:cs)    $ B.TText cp 0 $ '<' : rv w

    angleToken cs cp ('c' : char)
        | all isCode char   = case mapM B.readInt $ B.omit null $ B.divide '-' char of
                                Just ns  ->  next gen cs $ B.TText cp 3 $ map Char.chr ns
                                Nothing  ->  next gen cs $ B.TText cp (-1) char
    angleToken cs cp ""     = next gen cs $ B.TText cp 0 "<>"
    angleToken cs cp key    = case lookup key B.bracketKeywords of
                                Just w   ->  next gen cs $ B.TText cp 3 w
                                Nothing  ->  next gen cs $ B.TText cp (-1) key


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

isCode :: Char -> Bool
isCode '-' = True
isCode c   = Char.isDigit c


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

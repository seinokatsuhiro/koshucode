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
tokenLines = B.codeRollUp general

-- tokenLines :: B.Resource -> String -> [TokenLine]
-- tokenLines = B.codeLines . nextToken

general :: B.Map (B.CodeRoll B.Token)
general r@B.CodeRoll { B.codeInputPt = cp
                     , B.codeInput   = cs0
                     } = gen cs0 where

    io = inout r

    gen ""                           =  input r ""
    gen (c:'|':cs) | isOpen c        =  io cs            $ B.TOpen    cp [c, '|']
        
    gen (c:cs)     | c == '*'        =  ast   cs [c]
                   | c == '<'        =  bra   cs [c]
                   | c == '>'        =  cket  cs [c]
                   | c == '@'        =  slot  cs [c]
                   | c == '|'        =  bar   cs [c]
                   | c == '#'        =  hash  cs [c]
                   | isOpen c        =  io    cs         $ B.TOpen    cp [c]
                   | isClose c       =  io    cs         $ B.TClose   cp [c]
                   | isSingle c      =  io    cs         $ B.TText    cp 0 [c]
                   | isTerm c        =  term  cs [] ""
                   | isQQ c          =  qq    cs ""
                   | isQ c           =  q     cs ""
                   | isShort c       =  short cs [c]
                   | isWord c        =  word  cs [c]
                   | isSpace c       =  space cs 1
                   | otherwise       =  io    cs         $ B.TUnknown cp [c]

    ast (c:cs) w   | w == "****"     =  io  cs           $ B.TText    cp 0 w
                   | c == '*'        =  ast cs $ c:w
    ast cs w       | w == "*"        =  io  cs           $ B.TText    cp 0 w
                   | w == "**"       =  io  ""           $ B.TComment cp cs
                   | w == "***"      =  io  ""           $ B.TComment cp cs
                   | otherwise       =  io  cs           $ B.TUnknown cp w

    bra (c:cs) w   | c == '<'        =  bra   cs $ c:w
    bra cs w       | w == "<"        =  angle cs ""
                   | w == "<<"       =  io    cs         $ B.TOpen    cp w
                   | w == "<<<"      =  (io   cs         $ B.TOpen    cp w) `change` interp
                   | otherwise       =  io    cs         $ B.TUnknown cp w

    cket (c:cs) w  | c == '>'        =  cket cs $ c:w
    cket cs w      | w == ">"        =  word cs w
                   | w == ">>"       =  io   cs          $ B.TClose   cp w
                   | w == ">>>"      =  io   cs          $ B.TClose   cp w
                   | otherwise       =  io   cs          $ B.TUnknown cp w

    slot (c:cs) w  | c == '@'        =  slot cs $ c:w
                   | c == '\''       =  slotName cs     0 ""
                   | w == "@"        =  slotName (c:cs) 1 ""
                   | w == "@@"       =  slotName (c:cs) 2 ""
    slot cs w                        =  io cs            $ B.TUnknown cp w

    slotName (c:cs) n w | isWord c   =  slotName cs n $ c:w
    slotName cs n w                  =  io cs            $ B.TSlot cp n $ rv w

    hash ('!':cs) _                  =  io ""            $ B.TComment cp cs
    hash cs w                        =  io cs            $ B.TText    cp 0 w

    term (c:cs) ns w | isTerm c      =  term cs (rv w : ns) ""
                     | isWord c      =  term cs ns $ c:w
    term cs ns w                     =  io   cs          $ B.TTerm cp 0 $ rv (rv w : ns)

    qq (c:cs) w      | isQQ c        =  io cs            $ B.TText cp 2 $ rv w
                     | otherwise     =  qq cs $ c:w
    qq _ w                           =  io ""            $ B.TUnknown cp w

    q (c:cs) w       | isWord c      =  q cs $ c:w
    q cs w                           =  io cs            $ B.TText cp 1 $ rv w

    short (c:cs) w   | c == '.'      =  shortBody cs (rv w) ""
                     | isWord c      =  short cs $ c:w
    short cs w                       =  io cs            $ B.TText cp 0 $ rv w

    shortBody (c:cs) pre w | isWord c  = shortBody cs pre $ c:w
    shortBody cs pre w                 = io cs           $ B.TShort cp pre $ rv w

    word (c:cs) w    | isWord c      =  word cs $ c:w
    word cs w                        =  io cs            $ B.TText cp 0 $ rv w

    space (c:cs) n   | isSpace c     =  space cs $ n + 1
    space cs n                       =  io cs            $ B.TSpace cp n

    bar [] w                         =  io ""            $ B.TText cp 0 w
    bar (c:cs) w | c == '|'          =  bar cs $ c:w
                 | w == "||"         =  let cs' = B.trimLeft (c:cs)
                                        in io cs'         $ B.TText cp 0 w
                 | w == "|" &&
                   isClose c         =  io cs            $ B.TClose   cp ['|', c]
                 | w == "|"          =  io (c:cs)        $ B.TText    cp 0 w
                 | otherwise         =  io (c:cs)        $ B.TUnknown cp w

    angle (c:cs) w | c == '>'        =  angleToken cs $ rv w
                   | isWord c        =  angle cs $ c:w
    angle cs w                       =  io cs            $ B.TText cp 0 $ '<' : rv w

    angleToken cs ('c' : char)
        | all isCode char  = case mapM B.readInt $ B.omit null $ B.divide '-' char of
                               Just ns  ->  io cs $ B.TText cp 3 $ map Char.chr ns
                               Nothing  ->  io cs $ B.TText cp (-1) char
    angleToken cs ""       = io cs $ B.TText cp 0 "<>"
    angleToken cs key      = case lookup key B.bracketKeywords of
                               Just w   ->  io cs $ B.TText cp 3 w
                               Nothing  ->  io cs $ B.TText cp (-1) key

interp :: B.Map (B.CodeRoll B.Token)
interp r@B.CodeRoll { B.codeInputPt = cp
                    , B.codeInput   = cs0
                    } = int cs0 where

    io = inout r

    int ""                               =  r
    int (c:cs)    | isSpace c            =  space cs 1
                  | isTerm c             =  term cs [] ""
                  | otherwise            =  word (c:cs) ""

    word cs@('>':'>':'>':_) w            =  (io cs    $ B.TText cp 0 (rv w)) `change` general
    word (c:cs) w | isSpace c            =  io (c:cs) $ B.TText cp 0 (rv w)
                  | isTerm c             =  io (c:cs) $ B.TText cp 0 (rv w)
                  | otherwise            =  word cs   $ c:w
    word cs w                            =  io cs     $ B.TText cp 0 (rv w)

    term (c:cs) ns w | isTerm c          =  term cs (rv w : ns) ""
                     | isWord c          =  term cs ns $ c:w
    term cs ns w                         =  io cs     $ B.TTerm cp 0 $ rv (rv w : ns)

    space (c:cs) n   | isSpace c         =  space cs  $ n + 1
    space cs n                           =  io cs     $ B.TSpace cp n

rv :: B.Map [a]
rv = reverse

inout :: B.CodeRoll a -> String -> a -> B.CodeRoll a
inout r cs tok = r { B.codeInput = cs, B.codeOutput = tok : B.codeOutput r }

input :: B.CodeRoll a -> String -> B.CodeRoll a
input r cs = r { B.codeInput = cs }

change :: B.CodeRoll a -> B.Map (B.CodeRoll a) -> B.CodeRoll a
change r f = r { B.codeMap = f }


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


-- ----------------------  Char category

--  ( ) [ ] { } | : " ' # /
--  O C O C O C S S Q Q H T

-- Punctuations
isOpen, isClose, isSingle, isQ, isQQ, isTerm, isSpace, isWord :: B.Pred Char
isOpen     =  ( `elem` "([{" )  --  UnicodePunctuation
isClose    =  ( `elem` "}])" )  --  UnicodePunctuation
isSingle   =  ( `elem` ":|"  )  --  UnicodePunctuation | UnicodeSymbol
isQ        =  (    ==  '\''  )  --  UnicodePunctuation
isQQ       =  (    ==  '"'   )  --  UnicodePunctuation
isTerm     =  (    ==  '/'   )  --  UnicodePunctuation
isSpace    =  Char.isSpace      --  UnicodeSeprator | UnicodeOther
isWord  c  =  case B.generalCategoryGroup c of
                B.UnicodeLetter  ->  True
                B.UnicodeNumber  ->  True
                _                ->  c `elem` "-+=.~<#>_"

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

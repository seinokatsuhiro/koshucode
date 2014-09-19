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
import qualified Koshucode.Baala.Base.Abort          as B
import qualified Koshucode.Baala.Base.Prelude        as B
import qualified Koshucode.Baala.Base.Syntax         as B
import qualified Koshucode.Baala.Base.Text           as B
import qualified Koshucode.Baala.Base.Token.Bracket  as B
import qualified Koshucode.Baala.Base.Token.Short    as B
import qualified Koshucode.Baala.Base.Token.Token    as B
import qualified Koshucode.Baala.Base.Message        as Message



-- ----------------------  Tokenizer

-- | Token list on a line.
type TokenLine = B.CodeLine B.Token
type TokenRoll = B.CodeRoll B.Token

-- | Split string into list of tokens.
--   Result token list does not contain newline characters.
tokens :: B.Resource -> String -> B.Ab [B.Token]
tokens res cs = do ls <- tokenLines res cs
                   Right $ concatMap B.lineTokens ls

-- | Tokenize text.
tokenLines :: B.Resource -> String -> B.Ab [TokenLine]
tokenLines = B.codeRollUp general

-- | Split a next token from source text.
general :: B.AbMap TokenRoll
general r@B.CodeRoll { B.codeInputPt = cp
                     , B.codeInput   = cs0
                     } = ab $ gen cs0 where

    v           = scan r
    u   cs tok  = Right $ B.codeUpdate cs tok r
    int cs tok  = Right $ B.codeChange interp $ B.codeUpdate cs tok r
    ab          = Message.abToken [cp]

    gen ""                           =  Right r
    gen (c:'|':cs) | isOpen c        =  u cs            $ B.TOpen    cp [c, '|']
        
    gen (c:cs)     | c == '*'        =  ast   cs [c]
                   | c == '<'        =  bra   cs [c]
                   | c == '>'        =  cket  cs [c]
                   | c == '@'        =  slot  cs 1
                   | c == '|'        =  bar   cs [c]
                   | c == '#'        =  hash  cs [c]
                   | isOpen c        =  u     cs        $ B.TOpen    cp [c]
                   | isClose c       =  u     cs        $ B.TClose   cp [c]
                   | isSingle c      =  u     cs        $ B.TText    cp 0 [c]
                   | isTerm c        =  v               $ scanTerm   cp cs
                   | isQQ c          =  v               $ scanQQ     cp cs
                   | isQ c           =  v               $ scanQ      cp cs
                   | isShort c       =  short cs [c]
                   | isCode c        =  v               $ scanCode   cp (c:cs)
                   | isSpace c       =  v               $ scanSpace  cp cs
                   | otherwise       =  Message.forbiddenInput $ B.shortEmpty [c]

    ast (c:cs) w   | w == "****"     =  u    (c:cs)     $ B.TText    cp 0 w
                   | c == '*'        =  ast  cs         $ c:w
    ast cs w       | w == "**"       =  u    ""         $ B.TComment cp cs
                   | w == "***"      =  u    ""         $ B.TComment cp cs
                   | otherwise       =  u    cs         $ B.TText    cp 0 w

    bra (c:cs) w   | c == '<'        =  bra  cs         $ c:w
    bra cs w       | w == "<"        =  ang  cs ""
                   | w == "<<"       =  u    cs         $ B.TOpen    cp w
                   | w == "<<<"      =  int  cs         $ B.TOpen    cp w
                   | otherwise       =  Message.unkBracketText w

    cket (c:cs) w  | c == '>'        =  cket cs         $ c:w
    cket cs w      | w == ">"        =  v               $ scanCode   cp ('>':cs)
                   | w == ">>"       =  u    cs         $ B.TClose   cp w
                   | w == ">>>"      =  u    cs         $ B.TClose   cp w
                   | otherwise       =  Message.unkBracketText w

    slot (c:cs) n  | c == '@'        =  slot cs         $ n + 1
                   | c == '\''       =  v               $ scanSlot 0 cp cs  -- positional
    slot cs n                        =  v               $ scanSlot n cp cs

    hash ('!':cs) _                  =  u ""            $ B.TComment cp cs
    hash cs w                        =  u cs            $ B.TText    cp 0 w

    short (c:cs) w   | c == '.'      =  let pre = rv w
                                            (cs', body) = nextCode   cs
                                        in u cs'        $ B.TShort   cp pre body
                     | isCode c      =  short cs        $ c:w
    short cs w                       =  u cs            $ B.TText    cp 0 $ rv w

    bar [] w                         =  u ""            $ B.TText    cp 0 w
    bar (c:cs) w | c == '|'          =  bar cs          $ c:w
                 | w == "||"         =  let cs' = B.trimLeft (c:cs)
                                        in u cs'        $ B.TText    cp 0 w
                 | w == "|" &&
                   isClose c         =  u cs            $ B.TClose   cp ['|', c]
                 | otherwise         =  u (c:cs)        $ B.TText    cp 0 w

    ang (c:cs) w | c == '>'          =  u     cs        $ angle $ rv w
                 | isCode c          =  ang   cs        $ c:w
    ang cs w                         =  u     cs        $ B.TText    cp 0 $ '<' : rv w

    angle ('c' : s)
        | isCharCode s  =  case charCodes s of
                             Just ns  ->  B.TText cp 3 $ map Char.chr ns
                             Nothing  ->  B.TText cp (-1) s
    angle ""            =                 B.TText cp 0 "<>"
    angle s             =  case lookup s B.bracketKeywords of
                             Just w   ->  B.TText cp 3 w
                             Nothing  ->  B.TText cp (-1) s

charCodes :: String -> Maybe [Int]
charCodes = mapM B.readInt . B.omit null . B.divide '-'

interp :: B.AbMap TokenRoll
interp r@B.CodeRoll { B.codeInputPt = cp
                    , B.codeInput   = cs0
                    } = ab $ int cs0 where

    v           = scan r
    u   cs tok  = Right $ B.codeUpdate cs tok r
    gen cs tok  = Right $ B.codeChange general $ B.codeUpdate cs tok r
    ab          = Message.abToken [cp]

    int ""                           =  Right r
    int (c:cs)    | isSpace c        =  v         $ scanSpace cp cs
                  | isTerm c         =  v         $ scanTerm cp cs
                  | otherwise        =  word (c:cs) ""

    word cs@('>':'>':'>':_) w        =  gen cs    $ B.TText cp 0 (rv w)
    word (c:cs) w | isSpace c        =  u (c:cs)  $ B.TText cp 0 (rv w)
                  | isTerm c         =  u (c:cs)  $ B.TText cp 0 (rv w)
                  | otherwise        =  word cs   $ c:w
    word cs w                        =  u cs      $ B.TText cp 0 (rv w)

 
-- ----------------------  Scanner

type Next   a = String -> (String, a)
type AbNext a = String -> B.Ab (String, a)
type Scan     = B.CodePt -> String -> B.Ab (String, B.Token)

rv :: B.Map [a]
rv = reverse

scan :: TokenRoll -> B.Ab (String, B.Token) -> B.Ab TokenRoll
scan r (Right (cs, tok)) = Right $ B.codeUpdate cs tok r
scan _ (Left message)    = Left message

nextCode :: Next String
nextCode = loop "" where
    loop w cs@('>':'>':_)         =  (cs, rv w)
    loop w cs@('<':'<':_)         =  (cs, rv w)
    loop w (c:cs) | isCode c      =  loop (c:w) cs
    loop w cs                     =  (cs, rv w)

nextQQ :: AbNext String
nextQQ = loop "" where
    loop w (c:cs) | isQQ c        =  Right (cs, rv w)
                  | otherwise     =  loop (c:w) cs
    loop _ _                      =  Message.quotNotEnd

scanSpace :: Scan
scanSpace cp = loop 1 where
    loop n (c:cs) | isSpace c     =  loop (n + 1) cs
    loop n cs                     =  Right (cs, B.TSpace cp n)

scanCode :: Scan
scanCode cp cs = let (cs', w) = nextCode cs
                 in  Right (cs', B.TText cp 0 w)

scanQ :: Scan
scanQ cp cs =    let (cs', w) = nextCode cs
                 in Right (cs', B.TText cp 1 w)

scanQQ :: Scan
scanQQ cp cs = do (cs', w) <- nextQQ cs
                  Right (cs', B.TText cp 2 w)

scanTerm :: Scan
scanTerm cp = word [] where
    word ns (c:cs) | isCode c  =  let (cs', w) = nextCode (c:cs)
                                  in term (w : ns) cs'
                   | isQQ c    =  do (cs', w) <- nextQQ cs
                                     term (w : ns) cs'
    word _ _                   =  Message.forbiddenTerm

    term ns (c:cs) | isTerm c  =  word ns cs
    term ns cs                 =  Right (cs, B.TTerm cp 0 $ rv ns)

scanSlot :: Int -> Scan
scanSlot n cp cs = let (cs', w) = nextCode cs
                   in Right (cs', B.TSlot cp n w)


-- ----------------------  Char category

--  ( ) [ ] { } | : " ' # /
--  O C O C O C S S Q Q H T

-- Punctuations
isOpen, isClose, isSingle, isQ, isQQ, isTerm, isSpace, isCode :: B.Pred Char
isOpen     =  ( `elem` "([{" )  --  UnicodePunctuation
isClose    =  ( `elem` "}])" )  --  UnicodePunctuation
isSingle   =  ( `elem` ":|"  )  --  UnicodePunctuation | UnicodeSymbol
isQ        =  (    ==  '\''  )  --  UnicodePunctuation
isQQ       =  (    ==  '"'   )  --  UnicodePunctuation
isTerm     =  (    ==  '/'   )  --  UnicodePunctuation
isSpace    =  Char.isSpace
isCode     =  B.isCodeChar

isShortPrefix :: B.Pred String
isShortPrefix = all isShort

isShort :: B.Pred Char
isShort = Char.isAlpha

isCharCode :: B.Pred String
isCharCode = all isFigure

isFigure :: B.Pred Char
isFigure '-' = True
isFigure c   = Char.isDigit c


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

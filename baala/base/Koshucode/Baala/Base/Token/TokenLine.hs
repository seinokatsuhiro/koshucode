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
import qualified Koshucode.Baala.Base.Token.Bracket  as B
import qualified Koshucode.Baala.Base.Token.Short    as B
import qualified Koshucode.Baala.Base.Token.Token    as B



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

-- | Split a next token from source text.
general :: B.Map (B.CodeRoll B.Token)
general r@B.CodeRoll { B.codeInputPt = cp
                     , B.codeInput   = cs0
                     } = gen cs0 where

    io = inout r
    io' (cs, tok) = io cs tok

    gen ""                           =  input r ""
    gen (c:'|':cs) | isOpen c        =  io cs            $ B.TOpen    cp [c, '|']
        
    gen (c:cs)     | c == '*'        =  ast   cs [c]
                   | c == '<'        =  bra   cs [c]
                   | c == '>'        =  cket  cs [c]
                   | c == '@'        =  slot  cs 1
                   | c == '|'        =  bar   cs [c]
                   | c == '#'        =  hash  cs [c]
                   | isOpen c        =  io    cs         $ B.TOpen    cp [c]
                   | isClose c       =  io    cs         $ B.TClose   cp [c]
                   | isSingle c      =  io    cs         $ B.TText    cp 0 [c]
                   | isTerm c        =  io'              $ scanTerm   cp cs
                   | isQQ c          =  io'              $ scanQQ     cp cs
                   | isQ c           =  io'              $ scanQ      cp cs
                   | isShort c       =  short cs [c]
                   | isCode c        =  io'              $ scanCode   cp (c:cs)
                   | isSpace c       =  io'              $ scanSpace  cp cs
                   | otherwise       =  io    cs         $ B.TUnknown cp [c]

    ast (c:cs) w   | w == "****"     =  io  (c:cs)       $ B.TText    cp 0 w
                   | c == '*'        =  ast cs $ c:w
    ast cs w       | w == "**"       =  io  ""           $ B.TComment cp cs
                   | w == "***"      =  io  ""           $ B.TComment cp cs
                   | otherwise       =  io  cs           $ B.TText    cp 0 w

    bra (c:cs) w   | c == '<'        =  bra   cs $ c:w
    bra cs w       | w == "<"        =  angle cs ""
                   | w == "<<"       =  io    cs         $ B.TOpen    cp w
                   | w == "<<<"      =  (io   cs         $ B.TOpen    cp w) `change` interp
                   | otherwise       =  io    cs         $ B.TUnknown cp w

    cket (c:cs) w  | c == '>'        =  cket cs $ c:w
    cket cs w      | w == ">"        =  io'              $ scanCode   cp ('>':cs)
                   | w == ">>"       =  io   cs          $ B.TClose   cp w
                   | w == ">>>"      =  io   cs          $ B.TClose   cp w
                   | otherwise       =  io   cs          $ B.TUnknown cp w

    slot (c:cs) n  | c == '@'        =  slot cs $ n + 1
                   | c == '\''       =  io'              $ scanSlot 0 cp cs  -- positional
    slot cs n                        =  io'              $ scanSlot n cp cs

    hash ('!':cs) _                  =  io ""            $ B.TComment cp cs
    hash cs w                        =  io cs            $ B.TText    cp 0 w

    short (c:cs) w   | c == '.'      =  let pre = rv w
                                            Just (cs', body) = nextCode cs
                                        in io cs' $ B.TShort cp pre body
                     | isCode c      =  short cs $ c:w
    short cs w                       =  io cs            $ B.TText cp 0 $ rv w

    bar [] w                         =  io ""            $ B.TText cp 0 w
    bar (c:cs) w | c == '|'          =  bar cs $ c:w
                 | w == "||"         =  let cs' = B.trimLeft (c:cs)
                                        in io cs'         $ B.TText cp 0 w
                 | w == "|" &&
                   isClose c         =  io cs            $ B.TClose   cp ['|', c]
                 | w == "|"          =  io (c:cs)        $ B.TText    cp 0 w
                 | otherwise         =  io (c:cs)        $ B.TUnknown cp w

    angle (c:cs) w | c == '>'        =  angleToken cs $ rv w
                   | isCode c        =  angle cs $ c:w
    angle cs w                       =  io cs            $ B.TText cp 0 $ '<' : rv w

    angleToken cs ('c' : char)
        | all isFigure char  = case mapM B.readInt $ B.omit null $ B.divide '-' char of
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
    io' (cs, tok) = io cs tok

    int ""                               =  r
    int (c:cs)    | isSpace c            =  io' $ scanSpace cp cs
                  | isTerm c             =  io' $ scanTerm cp cs
                  | otherwise            =  word (c:cs) ""

    word cs@('>':'>':'>':_) w            =  (io cs    $ B.TText cp 0 (rv w)) `change` general
    word (c:cs) w | isSpace c            =  io (c:cs) $ B.TText cp 0 (rv w)
                  | isTerm c             =  io (c:cs) $ B.TText cp 0 (rv w)
                  | otherwise            =  word cs   $ c:w
    word cs w                            =  io cs     $ B.TText cp 0 (rv w)


-- ----------------------  

type Next a = String -> Maybe (String, a)
type Scan = B.CodePt -> String -> (String, B.Token)

unknown :: B.CodePt -> (String, B.Token)
unknown cp = ("", B.TUnknown cp "")

nextCode :: Next String
nextCode = loop "" where
    loop w cs@('>':'>':_)         =  Just (cs, rv w)
    loop w cs@('<':'<':_)         =  Just (cs, rv w)
    loop w (c:cs) | isCode c      =  loop (c:w) cs
    loop w cs                     =  Just (cs, rv w)

nextQQ :: Next String
nextQQ = loop "" where
    loop w (c:cs) | isQQ c        =  Just (cs, rv w)
                  | otherwise     =  loop (c:w) cs
    loop _ _                      =  Nothing

scanSpace :: Scan
scanSpace cp = loop 1 where
    loop n (c:cs) | isSpace c     =  loop (n + 1) cs
    loop n cs                     =  (cs, B.TSpace cp n)

scanCode :: Scan
scanCode cp cs = case nextCode cs of
                   Just (cs', w)  -> (cs', B.TText cp 0 w)
                   Nothing        -> unknown cp

scanQ :: Scan
scanQ cp cs = case nextCode cs of
                Just (cs', w)  -> (cs', B.TText cp 1 w)
                Nothing        -> unknown cp

scanQQ :: Scan
scanQQ cp cs = case nextQQ cs of
                 Just (cs', w) -> (cs', B.TText cp 2 w)
                 Nothing       -> unknown cp

scanTerm :: Scan
scanTerm cp = word [] where
    word ns (c:cs) | isCode c  =  case nextCode (c:cs) of
                                    Just (cs', w) -> term (w : ns) cs'
                                    Nothing       -> unknown cp
                   | isQQ c    =  case nextQQ cs of
                                    Just (cs', w) -> term (w : ns) cs'
                                    Nothing       -> unknown cp
    word _ _                   =  unknown cp

    term ns (c:cs) | isTerm c  =  word ns cs
    term ns cs                 =  (cs, B.TTerm cp 0 $ rv ns)

scanSlot :: Int -> Scan
scanSlot n cp cs = case nextCode cs of
                     Just (cs', w) -> (cs', B.TSlot cp n w)
                     Nothing       -> unknown cp

rv :: B.Map [a]
rv = reverse

inout :: B.CodeRoll a -> String -> a -> B.CodeRoll a
inout r cs tok = r { B.codeInput = cs, B.codeOutput = tok : B.codeOutput r }

input :: B.CodeRoll a -> String -> B.CodeRoll a
input r cs = r { B.codeInput = cs }

change :: B.CodeRoll a -> B.Map (B.CodeRoll a) -> B.CodeRoll a
change r f = r { B.codeMap = f }


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

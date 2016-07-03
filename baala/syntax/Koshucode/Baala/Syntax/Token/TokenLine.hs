{-# OPTIONS_GHC -Wall #-}

-- | Tokenizer of koshucode.

module Koshucode.Baala.Syntax.Token.TokenLine
  (
    -- * Library
    TokenLine,
    tokenLines, tokenLinesBz,
    tokens, toks,
    isShortPrefix,
  
    -- * Document
  
    -- ** Token type
    -- $TokenType
  
    -- ** Asterisks
    -- $Asterisks
  
    -- ** Examples
    -- $Examples
  ) where

import qualified Data.Map                               as Map
import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Pattern   as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Token.Section   as S
import qualified Koshucode.Baala.Base.Message           as Msg
import qualified Koshucode.Baala.Syntax.Symbol.Message  as Msg
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- ----------------------  Tokenizer

-- | Token list on a line.
type TokenLine = B.CodeLine S.Token

-- | Split string into list of tokens.
--   Result token list does not contain newline characters.
tokens :: B.NIOPoint -> S.InputText -> [S.Token]
tokens res cs = concatMap B.lineTokens $ tokenLines res cs

-- | Abbreviated tokenizer.
toks :: S.InputText -> [S.Token]
toks s = tokens (B.nioFrom $ B.stringBz s) s

-- | Tokenize text.
tokenLines :: B.NIOPoint -> S.InputText -> [TokenLine]
tokenLines = B.codeRollUp $ sectionRel changeSection

tokenLinesBz :: B.NIOPoint -> B.Bz -> [TokenLine]
tokenLinesBz = B.codeRollUpBz $ sectionRel changeSection

changeSection :: S.ChangeSection
changeSection name =
    case name of
      "rel"      -> Just $ B.codeChange $ sectionRel changeSection
      "note"     -> Just $ B.codeChange $ S.sectionNote changeSection
      "end"      -> Just $ B.codeChange S.sectionEnd
      "license"  -> Just $ B.codeChange $ S.sectionLicense changeSection
      "local"    -> Just $ sectionUnsupported "local section"
      "attr"     -> Just $ sectionUnsupported "attr section"
      "text"     -> Just $ sectionUnsupported "text section"
      "doc"      -> Just $ sectionUnsupported "doc section"
      "data"     -> Just $ sectionUnsupported "data section"
      _          -> Nothing

sectionUnsupported :: String -> S.TokenRollMap
sectionUnsupported msg r@B.CodeRoll { B.codeInput = cs } = B.codeUpdate "" tok r where
    tok  = S.unknownToken cp cs $ Msg.unsupported msg
    cp   = B.codeInputPt r


-- ----------------------  Relational section

uncons3 :: a -> (Int -> a -> a -> a -> [a] -> [a] -> [a] -> b) -> [a] -> b
uncons3 z f = first where
    first (a:bs)            = second a bs
    first []                = f 0 z z z [] [] []

    second a bs@(b:cs)      = third a b bs cs
    second a []             = f 1 a z z [] [] []

    third a b bs cs@(c:ds)  = f 3 a b c bs cs ds
    third a b bs []         = f 2 a b z bs [] []

-- | Split a next token from source text.
sectionRel :: S.ChangeSection -> S.TokenRollMap
sectionRel change r@B.CodeRoll { B.codeInputPt = cp, B.codeWords = wtab } = r' where

    v              = S.scan r
    vw             = S.scanW r
    up             = u ""
    u   cs tok     = B.codeUpdate cs tok r
    int cs tok     = B.codeChange (interp change) $ B.codeUpdate cs tok r

    sign '+'       = GT
    sign  _        = LT

    -- ----------------------  dispatch

    r' = S.section change (uncons3 '\0' dispatch) r

    dispatch n a b c bs cs ds
        | S.isSpace a            = v               $ S.scanSpace  cp bs
        | isTerm a               = vw              $ scanTermPath cp wtab bs
        | isPM a && isTerm b     = vw              $ scanTermSign (sign a) cp wtab cs
        | isQQ a                 = v               $ scanQQ      cp bs
        | isQ a && isTerm b      = vw              $ scanTermQ   cp wtab cs
        | isQ a                  = vw              $ scanQ       cp wtab bs

        | a == '(' && c == ')' && b `elem` "+-/=#"
                                 = u ds            $ S.TTextRaw   cp [a,b,c]
        | a == '{' && b == '|'   = int cs          $ S.TOpen      cp [a,b]
        | isOpen a && isGrip b   = u cs            $ S.TOpen      cp [a,b]
        | isGrip a && isClose b  = u cs            $ S.TClose     cp [a,b]
        | isOpen a               = u bs            $ S.TOpen      cp [a]
        | isClose a              = u bs            $ S.TClose     cp [a]

        | a == '*'               = aster bs [a]
        | a == '<'               = angle bs [a]
        | a == '@'               = at    bs 1
        | a == '|'               = bar   bs [a]
        | a == '^'               = hat   bs
        | a == '#' && b == '!'   = up              $ S.TComment   cp bs
        | a == '-' && b == '*' && c == '-'
                                 = up              $ S.TComment   cp bs

        | isSingle a             = u bs            $ S.TTextRaw   cp [a]
        | S.isSymbol a           = vw              $ S.scanSymbol cp wtab $ a : bs
        | n == 0                 = r
        | otherwise              = u []            $ S.unknownToken cp cs
                                                   $ Msg.forbiddenInput $ S.angleQuote [a]

    -- ----------------------  begin with "@"

    at (c:cs) n | c == '@'    = at cs           $ n + 1
                | c == '\''   = v               $ scanSlot 0 cp cs  -- positional
    at cs n                   = v               $ scanSlot n cp cs

    -- ----------------------  begin with "*"

    aster (c:cs) w
        | w == "****"         = u (c:cs)        $ S.TTextRaw cp w
        | c == '*'            = aster cs (c:w)
    aster cs w
        | w == "**"           = up              $ S.TComment cp cs
        | w == "***"          = up              $ S.TComment cp cs
        | otherwise           = vw              $ S.scanSymbol cp wtab $ w ++ cs

    -- ----------------------  begin with "^"

    -- read local reference, like ^/g
    hat ('/' : cs)                   = localToken cs S.LocalNest
    hat cs@(c : _) | S.isSymbol c    = localToken cs S.LocalSymbol
    hat cs                           = u [] $ S.unknownToken cp cs $ Msg.adlib "local"

    localToken cs k                  = case S.nextSymbolPlain cs of
                                         Right (cs', w) -> u cs' $ S.TLocal cp (k w) (-1) []
                                         Left a         -> u []  $ S.TUnknown cp cs a

    -- ----------------------  begin with "|"

    bar (c:cs) w
        | c == '|'                   = bar cs (c:w)
        | w == "|" && isJudge c      = judge cs [c, '|']
        | w == "|" && S.isSymbol c   = clock cs [c, '|']
    bar cs w                         = let cs' = B.trimLeft cs
                                       in u cs'        $ S.TTextRaw cp w

    -- read judgement sign, like |--, |-x
    judge (c:cs) w
        | isJudge c || Ch.isAlpha c  = judge cs (c:w)
        | S.isSymbol c               = clock (c:cs) w
    judge cs w                       = u cs            $ S.TTextBar cp $ rv w

    -- read clock, like |03:30|
    clock (c:cs) w | c == '|'        = u cs            $ S.TTextBar cp $ rv (c:w)
                   | isClock c       = clock cs (c:w)
    clock cs w                       = u cs            $ S.TTextBar cp $ rv w

    -- ----------------------  begin with "<"

    angle (c:cs) w | c == '<'        = angle cs (c:w)
    angle cs w     | w == "<"        = angleMid cs ""
                   | otherwise       = u cs            $ S.TTextRaw cp w

    -- read keyword, like <crlf>
    angleMid (c:cs) w
        | c == '>'                   = u cs            $ angleToken $ rv w
        | S.isSymbol c               = angleMid cs (c:w)
    angleMid cs w                    = u cs            $ S.TTextRaw cp $ '<' : rv w

    angleToken ""                    = S.TTextRaw cp "<>"
    angleToken ('c' : s)
        | isCharCode s  = case charCodes s of
                            Just ns  -> S.TTextKey cp $ map Ch.chr ns
                            Nothing  -> S.TTextUnk cp s
    angleToken s        = case lookup s S.angleTexts of
                            Just w   -> S.TTextKey cp w
                            Nothing  -> S.TTextUnk cp s

charCodes :: S.InputText -> Maybe [Int]
charCodes = mapM B.readInt . B.omit null . B.divide '-'

-- interpretation content between {| and |}
interp :: S.ChangeSection -> S.TokenRollMap
interp change r@B.CodeRoll { B.codeInputPt = cp
                           , B.codeWords = wtab } = S.section change int r where

    v           = S.scan r
    vw          = S.scanW r
    u   cs tok  = B.codeUpdate cs tok r
    gen cs tok  = B.codeChange (sectionRel change) $ B.codeUpdate cs tok r

    int ""                           = r
    int (c:cs)    | S.isSpace c      = v         $ S.scanSpace  cp cs
                  | isTerm c         = vw        $ scanTermPath cp wtab cs
                  | otherwise        = word (c:cs) ""

    word cs@('|':'}':_) w            = gen cs    $ S.TTextRaw cp $ rv w
    word (c:cs) w | S.isSpace c      = u (c:cs)  $ S.TTextRaw cp $ rv w
                  | isTerm c         = u (c:cs)  $ S.TTextRaw cp $ rv w
                  | otherwise        = word cs   $ c:w
    word cs w                        = u cs      $ S.TTextRaw cp $ rv w


-- ----------------------  Scanner

rv :: B.Map [a]
rv = reverse

scanQ :: S.ScanW
scanQ cp wtab cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> S.symbolToken S.TTextQ w cp wtab cs'
      Left a         -> (wtab, [], S.TUnknown cp cs a)
          

-- | Scan double-quoted text.
scanQQ :: S.Scan
scanQQ cp cs = case S.nextQQ cs of
                  Right (cs', w) -> (cs', S.TTextQQ cp w)
                  Left a         -> ([], S.TUnknown cp cs a)

-- | Scan slot name, like @aaa.
scanSlot :: Int -> S.Scan
scanSlot n cp cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> (cs', S.TSlot cp n w)
      Left a         -> ([], S.TUnknown cp cs a)
      

-- | Scan signed term name
scanTermSign :: Ordering -> S.ScanW
scanTermSign = scanTerm S.TermTypePath

-- | Scan term name
scanTermPath :: S.ScanW
scanTermPath = scanTerm S.TermTypePath EQ

-- | Scan quoted term
scanTermQ :: S.ScanW
scanTermQ = scanTerm S.TermTypeQuoted EQ

-- Scan term name or term path
scanTerm :: S.TermType -> Ordering -> S.ScanW
scanTerm q sign cp wtab cs0 = word [] cs0 where
    word ns ccs@(c:cs)
        | c == '='      = call (S.nextSymbolPlain cs)  (\w -> nterm ns w)
        | S.isSymbol c  = call (S.nextSymbolPlain ccs) (\w -> term (w : ns))
        | isQQ c        = call (S.nextQQ cs)           (\w -> term (w : ns))
    word _ _            = (wtab, [], S.unknownToken cp cs0 Msg.expOrdSym)
    call e f            = case e of
                            Right (cs', w) -> f w cs'
                            Left a         -> (wtab, [], S.TUnknown cp cs0 a)

    nterm ns w cs'      = let n  = B.nioNumber $ B.codePtSource cp
                              w' = show n ++ ('=' : w)
                          in term (w' : ns) cs'

    term ns (c:cs) | isTerm c   = word ns cs
    term [n] cs | q == S.TermTypePath
                       = case Map.lookup n wtab of
                           Just n' -> (wtab, cs, S.TTermN cp sign n')
                           Nothing -> let wtab' = Map.insert n n wtab
                                      in (wtab', cs, S.TTermN cp sign n)
    term ns cs         = (wtab, cs, S.TTerm cp q $ rv ns)


-- ----------------------  Char category

-- Punctuations
isOpen, isClose, isGrip, isJudge, isSingle, isQ, isQQ, isTerm, isPM :: B.Pred Char
isOpen     = ( `elem` "([{"    )  -- Punctuation
isClose    = ( `elem` "}])"    )  -- Punctuation
isGrip     = ( `elem` "-=|?"   )  -- Punctuation | Symbol   -- :*+
isJudge    = ( `elem` "-="     )  -- Punctuation | Symbol
isSingle   = ( `elem` ":|#"    )  -- Punctuation | Symbol
isQ        = (    ==  '\''     )  -- Punctuation
isQQ       = (    ==  '"'      )  -- Punctuation
isTerm     = (    ==  '/'      )  -- Punctuation
isPM a     = (a == '+' || a == '-')

isFigure :: B.Pred Char
isFigure c     = c == '-' || Ch.isDigit c

isClock :: B.Pred Char
isClock c      = Ch.isDigit c || c `elem` ".:'+-"

isShortPrefix, isCharCode :: B.Pred String
isShortPrefix  = all S.isShort
isCharCode     = all isFigure



-- ------------------------------------------------------------------
-- $TokenType
--
--  [Bracket]   Open and closed brackets.
--              @(@ /group/ @)@ ,
--              @{@ /set/ @}@ ,
--              @[@ /list/ @]@ ,
--              @{-@ /tie/ @-}@ , and
--              @{=@ /relation/ @=}@
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
--  >>> toks $ unlines ["aa", "'bb'", "\"cc\""]
--  [ TText <I0-L1-C0> TextRaw "aa"
--  , TText <I0-L2-C0> TextQ "bb"
--  , TText <I0-L2-C3> TextQ ""
--  , TText <I0-L3-C0> TextQQ "cc" ]
--
--  Judgement.
--
--  >>> toks "|-- R  /a A0 /b 31"
--  [ TText <I0-L1-C0> TextBar "|--", TSpace <I0-L1-C3> 1
--  , TText <I0-L1-C4> TextRaw "R", TSpace <I0-L1-C5> 2
--  , TTermN <I0-L1-C7> EQ "a", TSpace <I0-L1-C9> 1
--  , TText <I0-L1-C10> TextRaw "A0", TSpace <I0-L1-C12> 1
--  , TTermN <I0-L1-C13> EQ "b", TSpace <I0-L1-C15> 1
--  , TText <I0-L1-C16> TextRaw "31" ]
--
--  Brackets.
--
--  >>> toks "aa (bb x y (z))"
--  [ TText <I0-L1-C0> TextRaw "aa"
--  , TSpace <I0-L1-C2> 1
--  , TOpen <I0-L1-C3> "("
--    , TText <I0-L1-C4> TextRaw "bb", TSpace <I0-L1-C6> 1
--    , TText <I0-L1-C7> TextRaw "x", TSpace <I0-L1-C8> 1
--    , TText <I0-L1-C9> TextRaw "y", TSpace <I0-L1-C10> 1
--    , TOpen <I0-L1-C11> "("
--      , TText <I0-L1-C12> TextRaw "z"
--    , TClose <I0-L1-C13> ")"
--  , TClose <I0-L1-C14> ")" ]
--
--  A comment.
--
--  >>> toks $ unlines ["abc ** this is a comment", "def",""]
--  [ TText <I0-L1-C0> TextRaw "abc", TSpace <I0-L1-C3> 1
--  , TComment <I0-L1-C4> " this is a comment"
--  , TText <I0-L2-C0> TextRaw "def" ]
--

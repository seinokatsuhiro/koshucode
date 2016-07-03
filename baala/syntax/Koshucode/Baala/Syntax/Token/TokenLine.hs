{-# LANGUAGE PatternSynonyms #-}
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
import qualified Koshucode.Baala.Syntax.Token.Utility   as S
import qualified Koshucode.Baala.Base.Message           as Msg
import qualified Koshucode.Baala.Syntax.Symbol.Message  as Msg
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- ----------------------  Tokenizer

-- | Token list on a line.
type TokenLine = B.CodeLine S.Token

-- | Code roll for token.
type TokenRoll = B.CodeRoll S.Token

-- | Read single token.
type TokenRollMap = B.Map TokenRoll

-- | Split string into list of tokens.
--   Result token list does not contain newline characters.
tokens :: B.NIOPoint -> S.InputText -> [S.Token]
tokens res cs = concatMap B.lineTokens $ tokenLines res cs

-- | Abbreviated tokenizer.
toks :: S.InputText -> [S.Token]
toks s = tokens (B.nioFrom $ B.stringBz s) s

-- | Tokenize text.
tokenLines :: B.NIOPoint -> S.InputText -> [TokenLine]
tokenLines = B.codeRollUp relation

tokenLinesBz :: B.NIOPoint -> B.Bz -> [TokenLine]
tokenLinesBz = B.codeRollUpBz relation

-- Line begins with the equal sign is treated as section delimter.
start :: (S.InputText -> TokenRoll) -> TokenRollMap
start f r@B.CodeRoll { B.codeMap    = prev
                     , B.codeInput  = cs0
                     , B.codeOutput = out } = st out cs0 where
    st [] ('=' : _) = B.codeChange (section prev) r
    st _ cs         = f cs

section :: TokenRollMap -> TokenRollMap
section prev r@B.CodeRoll { B.codeInputPt  = cp
                          , B.codeInput    = cs0
                          , B.codeWords    = ws
                          } = sec cs0 where
    v      = scan r
    vw     = scanW r
    out    = reverse $ S.sweepToken $ B.codeOutput r
    toPrev = B.codeChange prev

    sec ""                    = dispatch out  -- end of line
    sec ('*' : '*' : _)       = dispatch out  -- end of effective text
    sec ccs@(c:cs)
        | isSpace c     = v  $ scanSpace  cp cs
        | isSymbol c    = vw $ scanSymbol cp ws ccs
        | otherwise     = toPrev $ unexpSect [] r

    dispatch :: [S.Token] -> TokenRoll
    dispatch [S.TTextSect _] = B.codeChange prev r
    dispatch ts@[S.TTextSect _, S.TTextRaw _ name] =
        case name of
          "rel"      -> B.codeChange relation r
          "note"     -> B.codeChange note r
          "end"      -> B.codeChange end r
          "license"  -> B.codeChange license r
          "local"    -> toPrev $ unsupported "local section" r
          "attr"     -> toPrev $ unsupported "attr section" r
          "text"     -> toPrev $ unsupported "text section" r
          "doc"      -> toPrev $ unsupported "doc section" r
          "data"     -> toPrev $ unsupported "data section" r
          _          -> toPrev $ unexpSect ts r
    dispatch ts       = toPrev $ unexpSect ts r

unsupported :: String -> TokenRollMap
unsupported msg r@B.CodeRoll { B.codeInput = cs } = B.codeUpdate "" tok r where
    tok  = S.unknownToken cp cs $ Msg.unsupported msg
    cp   = B.codeInputPt r

unexpSect :: [S.Token] -> TokenRollMap
unexpSect ts r@B.CodeRoll { B.codeInput = cs } = B.codeUpdate "" tok r where
    tok  = S.unknownToken cp cs $ Msg.unexpSect help
    cp | null ts    = B.codeInputPt r
       | otherwise  = B.codePt $ head ts
    help = [ "=== rel      for relational calculation"
           , "=== note     for commentary section"
           , "=== license  for license section"
           , "=== end      for ending of input" ]

-- Tokenizer for end section.
end :: TokenRollMap
end r@B.CodeRoll { B.codeInput = cs } = comment cs r

-- Tokenizer for note section.
note :: TokenRollMap
note r = start (`comment` r) r

license :: TokenRollMap
license r = start (`textLicense` r) r

comment :: S.InputText -> TokenRollMap
comment "" r = r
comment cs r = B.codeUpdate "" tok r where
    tok  = S.TComment cp cs
    cp   = B.codeInputPt r

textLicense :: S.InputText -> TokenRollMap
textLicense "" r = r
textLicense cs r = B.codeUpdate "" tok r where
    tok  = S.TText cp S.TextLicense cs
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
relation :: TokenRollMap
relation r@B.CodeRoll { B.codeInputPt = cp, B.codeWords = wtab } = r' where

    v              = scan r
    vw             = scanW r
    up             = u ""
    u   cs tok     = B.codeUpdate cs tok r
    int cs tok     = B.codeChange interp $ B.codeUpdate cs tok r

    sign '+'       = GT
    sign  _        = LT

    -- ----------------------  dispatch

    r' = start (uncons3 '\0' dispatch) r

    dispatch n a b c bs cs ds
        | isSpace a              = v               $ scanSpace    cp bs
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
        | isSymbol a             = vw              $ scanSymbol  cp wtab $ a : bs
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
        | otherwise           = vw              $ scanSymbol cp wtab $ w ++ cs

    -- ----------------------  begin with "^"

    -- read local reference, like ^/g
    hat ('/' : cs)                   = localToken cs S.LocalNest
    hat cs@(c : _) | isSymbol c      = localToken cs S.LocalSymbol
    hat cs                           = u [] $ S.unknownToken cp cs $ Msg.adlib "local"

    localToken cs k                  = case S.nextSymbolPlain cs of
                                         Right (cs', w) -> u cs' $ S.TLocal cp (k w) (-1) []
                                         Left a         -> u []  $ S.TUnknown cp cs a

    -- ----------------------  begin with "|"

    bar (c:cs) w
        | c == '|'                   = bar cs (c:w)
        | w == "|" && isJudge c      = judge cs [c, '|']
        | w == "|" && isSymbol c     = clock cs [c, '|']
    bar cs w                         = let cs' = B.trimLeft cs
                                       in u cs'        $ S.TTextRaw cp w

    -- read judgement sign, like |--, |-x
    judge (c:cs) w
        | isJudge c || Ch.isAlpha c  = judge cs (c:w)
        | isSymbol c                 = clock (c:cs) w
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
        | isSymbol c                 = angleMid cs (c:w)
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
interp :: TokenRollMap
interp r@B.CodeRoll { B.codeInputPt = cp, B.codeWords = wtab } = start int r where

    v           = scan r
    vw          = scanW r
    u   cs tok  = B.codeUpdate cs tok r
    gen cs tok  = B.codeChange relation $ B.codeUpdate cs tok r

    int ""                           = r
    int (c:cs)    | isSpace c        = v         $ scanSpace    cp cs
                  | isTerm c         = vw        $ scanTermPath cp wtab cs
                  | otherwise        = word (c:cs) ""

    word cs@('|':'}':_) w            = gen cs    $ S.TTextRaw cp $ rv w
    word (c:cs) w | isSpace c        = u (c:cs)  $ S.TTextRaw cp $ rv w
                  | isTerm c         = u (c:cs)  $ S.TTextRaw cp $ rv w
                  | otherwise        = word cs   $ c:w
    word cs w                        = u cs      $ S.TTextRaw cp $ rv w


-- ----------------------  Scanner

type Scan  = B.CodePt -> S.InputText -> (S.InputText, S.Token)
type ScanW = B.CodePt -> B.WordTable -> S.InputText -> (B.WordTable, S.InputText, S.Token)

rv :: B.Map [a]
rv = reverse

scan :: TokenRoll -> (S.InputText, S.Token) -> TokenRoll
scan r (cs, tok) = B.codeUpdate cs tok r

-- Scan with word table.
scanW :: TokenRoll -> (B.WordTable, S.InputText, S.Token) -> TokenRoll
scanW r (wtab, cs, tok) = B.codeUpdateWords wtab cs tok r

symbolToken :: (B.CodePt -> String -> S.Token) -> String -> ScanW
symbolToken k w cp wtab cs =
    case Map.lookup w wtab of
         Just w' -> (wtab, cs, k cp w')
         Nothing -> let wtab' = Map.insert w w wtab
                    in (wtab', cs, k cp w)

scanSymbol :: ScanW
scanSymbol cp wtab cs =
    let (cs', sym) = S.nextSymbol cs
    in case sym of
         S.SymbolShort pre w  -> (wtab, cs', S.TShort cp pre w)
         S.SymbolCommon w     -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolGeneral w    -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolPlain w      -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolNumeric w    -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolUnknown w    -> (wtab, [], S.unknownToken cp cs $ Msg.forbiddenInput w)

scanSpace :: Scan
scanSpace cp cs =
    let (cs', n) = S.nextSpace cs
    in (cs', S.TSpace cp $ n + 1)

scanQ :: ScanW
scanQ cp wtab cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> symbolToken S.TTextQ w cp wtab cs'
      Left a         -> (wtab, [], S.TUnknown cp cs a)
          

-- | Scan double-quoted text.
scanQQ :: Scan
scanQQ cp cs = case S.nextQQ cs of
                  Right (cs', w) -> (cs', S.TTextQQ cp w)
                  Left a         -> ([], S.TUnknown cp cs a)

-- | Scan slot name, like @aaa.
scanSlot :: Int -> Scan
scanSlot n cp cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> (cs', S.TSlot cp n w)
      Left a         -> ([], S.TUnknown cp cs a)
      

-- | Scan signed term name
scanTermSign :: Ordering -> ScanW
scanTermSign = scanTerm S.TermTypePath

-- | Scan term name
scanTermPath :: ScanW
scanTermPath = scanTerm S.TermTypePath EQ

-- | Scan quoted term
scanTermQ :: ScanW
scanTermQ = scanTerm S.TermTypeQuoted EQ

-- Scan term name or term path
scanTerm :: S.TermType -> Ordering -> ScanW
scanTerm q sign cp wtab cs0 = word [] cs0 where
    word ns ccs@(c:cs)
        | c == '='      = call (S.nextSymbolPlain cs)  (\w -> nterm ns w)
        | isSymbol c    = call (S.nextSymbolPlain ccs) (\w -> term (w : ns))
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

isSymbol, isSpace, isShort :: B.Pred Char
isSymbol   = S.isSymbolChar
isSpace    = Ch.isSpace
isShort    = Ch.isAlpha

isFigure :: B.Pred Char
isFigure c     = c == '-' || Ch.isDigit c

isClock :: B.Pred Char
isClock c      = Ch.isDigit c || c `elem` ".:'+-"

isShortPrefix, isCharCode :: B.Pred String
isShortPrefix  = all isShort
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

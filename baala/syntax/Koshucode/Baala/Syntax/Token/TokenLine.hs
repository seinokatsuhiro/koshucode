{-# LANGUAGE DeriveDataTypeable #-}
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
  
    -- * Examples
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

-- | Split string into list of tokens.
--   Result token list does not contain newline characters.
tokens :: B.NIOPoint -> S.InputText -> B.Ab [S.Token]
tokens res cs = do ls <- tokenLines res cs
                   Right $ concatMap B.lineTokens ls

-- | Abbreviated tokenizer.
toks :: S.InputText -> B.Ab [S.Token]
toks s = tokens (B.nioFrom $ B.stringBz s) s

-- | Tokenize text.
tokenLines :: B.NIOPoint -> S.InputText -> B.Ab [TokenLine]
tokenLines = B.codeRollUp relation

tokenLinesBz :: B.NIOPoint -> B.Bz -> B.Ab [TokenLine]
tokenLinesBz = B.codeRollUpBz relation

-- Line begins with the equal sign is treated as section delimter.
start :: (S.InputText -> B.Ab TokenRoll) -> B.CodePt -> TokenRoll -> B.Ab TokenRoll
start f cp r@B.CodeRoll { B.codeMap    = prev
                        , B.codeInput  = cs0
                        , B.codeOutput = out } = st out cs0 where
    st [] ('=' : _) = Right $ B.codeChange (section prev) r
    st _ cs         = Msg.abToken [cp] $ f cs

start' :: (S.InputText -> TokenRoll) -> B.CodePt -> TokenRoll -> TokenRoll
start' f _ r@B.CodeRoll { B.codeMap    = prev
                         , B.codeInput  = cs0
                         , B.codeOutput = out } = st out cs0 where
    st [] ('=' : _) = B.codeChange (section prev) r
    st _ cs         = f cs

section :: B.AbMap TokenRoll -> B.AbMap TokenRoll
section prev r@B.CodeRoll { B.codeInputPt  = cp
                          , B.codeInput    = cs0
                          , B.codeWords    = ws
                          } = sec cs0 where
    v    = scan r
    vw   = scanW r
    out  = reverse $ S.sweepToken $ B.codeOutput r

    sec ""                    = dispatch out
    sec ('*' : '*' : _)       = dispatch out
    sec ccs@(c:cs)
        | isSpace c     = v  $ scanSpace cp cs
        | isSymbol c    = vw $ scanSymbol cp ws ccs
        | otherwise     = Msg.unexpSect help

    dispatch :: [S.Token] -> B.Ab TokenRoll
    dispatch [S.TTextSect _] = Right $ B.codeChange prev r
    dispatch [S.TTextSect _, S.TTextRaw _ name] =
        case name of
          "rel"      -> Right $ B.codeChange relation r
          "note"     -> Right $ B.codeChange note r
          "end"      -> Right $ B.codeChange end r
          "license"  -> Right $ B.codeChange license r
          "local"    -> Msg.unsupported "local section"
          "attr"     -> Msg.unsupported "attr section"
          "text"     -> Msg.unsupported "text section"
          "doc"      -> Msg.unsupported "doc section"
          "data"     -> Msg.unsupported "data section"
          _          -> Msg.unexpSect help
    dispatch _        = Msg.unexpSect help

    help = [ "=== rel      for relational calculation"
           , "=== note     for commentary section"
           , "=== license  for license section"
           , "=== end      for ending of input" ]

-- Tokenizer for end section.
end :: B.AbMap TokenRoll
end r@B.CodeRoll { B.codeInput = cs } = comment r cs

-- Tokenizer for note section.
note :: B.AbMap TokenRoll
note r@B.CodeRoll { B.codeInputPt = cp } = start (comment r) cp r

license :: B.AbMap TokenRoll
license r@B.CodeRoll { B.codeInputPt = cp } = start (textLicense r) cp r

comment :: TokenRoll -> S.InputText -> B.Ab TokenRoll
comment r "" = Right r
comment r cs = Right $ B.codeUpdate "" tok r where
    tok  = S.TComment cp cs
    cp   = B.codeInputPt r

textLicense :: TokenRoll -> S.InputText -> B.Ab TokenRoll
textLicense r "" = Right r
textLicense r cs = Right $ B.codeUpdate "" tok r where
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
relation :: B.AbMap TokenRoll
relation r@B.CodeRoll { B.codeInputPt = cp, B.codeWords = wtab } = r' where

    v              = scan r
    vw             = scanW r
    up             = u ""
    u   cs tok     = Right $ B.codeUpdate cs tok r
    int cs tok     = Right $ B.codeChange interp $ B.codeUpdate cs tok r

    sign '+'       = GT
    sign  _        = LT

    -- ----------------------  dispatch

    r' = start (uncons3 '\0' dispatch) cp r

    dispatch n a b c bs cs ds
        | isSpace a              = v               $ scanSpace    cp bs
        | isTerm a               = vw              $ scanTermPath cp wtab bs
        | isPM a && isTerm b     = vw              $ scanTermSign (sign a) cp wtab cs
        | isQQ a                 = v               $ scanQQ       cp bs
        | isQ a && isTerm b      = vw              $ scanTermQ    cp wtab cs
        | isQ a                  = vw              $ scanQ        cp wtab bs

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
        | isSymbol a             = vw              $ scanSymbol   cp wtab $ a : bs
        | n == 0                 = Right r
        | otherwise              = Msg.forbiddenInput $ S.angleQuote [a]

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
    hat _                            = Msg.adlib "local"

    localToken cs k                  = do (cs', w) <- S.nextSymbolPlain cs
                                          u cs' $ S.TLocal cp (k w) (-1) []

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

interp :: B.AbMap TokenRoll
interp = Right . interp'

-- interpretation content between {| and |}
interp' :: B.Map TokenRoll
interp' r@B.CodeRoll { B.codeInputPt = cp, B.codeWords = wtab } = start' int cp r where

    v           = scan' r
    vw          = scanW' r
    u   cs tok  = B.codeUpdate cs tok r
    gen cs tok  = B.codeChange relation $ B.codeUpdate cs tok r

    int ""                           = r
    int (c:cs)    | isSpace c        = v         $ scanSpace'    cp cs
                  | isTerm c         = vw        $ scanTermPath' cp wtab cs
                  | otherwise        = word (c:cs) ""

    word cs@('|':'}':_) w            = gen cs    $ S.TTextRaw cp $ rv w
    word (c:cs) w | isSpace c        = u (c:cs)  $ S.TTextRaw cp $ rv w
                  | isTerm c         = u (c:cs)  $ S.TTextRaw cp $ rv w
                  | otherwise        = word cs   $ c:w
    word cs w                        = u cs      $ S.TTextRaw cp $ rv w


-- ----------------------  Scanner

type Scan  = B.CodePt -> S.InputText -> B.Ab (S.InputText, S.Token)
type ScanW = B.CodePt -> B.WordTable -> S.InputText -> B.Ab (B.WordTable, S.InputText, S.Token)

type Scan'  = B.CodePt -> S.InputText -> (S.InputText, S.Token)
type ScanW' = B.CodePt -> B.WordTable -> S.InputText -> (B.WordTable, S.InputText, S.Token)

rv :: B.Map [a]
rv = reverse

scan :: TokenRoll -> B.Ab (S.InputText, S.Token) -> B.Ab TokenRoll
scan r (Right (cs, tok)) = Right $ B.codeUpdate cs tok r
scan _ (Left message)    = Left message

-- Scan with word table.
scanW :: TokenRoll -> B.Ab (B.WordTable, S.InputText, S.Token) -> B.Ab TokenRoll
scanW r (Right (wtab, cs, tok))  = Right $ B.codeUpdateWords wtab cs tok r
scanW _ (Left message)           = Left message

scan' :: TokenRoll -> (S.InputText, S.Token) -> TokenRoll
scan' r (cs, tok) = B.codeUpdate cs tok r

-- Scan with word table.
scanW' :: TokenRoll -> (B.WordTable, S.InputText, S.Token) -> TokenRoll
scanW' r (wtab, cs, tok) = B.codeUpdateWords wtab cs tok r

symbolToken :: (B.CodePt -> String -> S.Token) -> String -> ScanW
symbolToken k w cp wtab cs =
    case Map.lookup w wtab of
         Just w' -> Right (wtab, cs, k cp w')
         Nothing -> let wtab' = Map.insert w w wtab
                    in Right (wtab', cs, k cp w)

scanSymbol :: ScanW
scanSymbol cp wtab cs =
    let (cs', sym) = S.nextSymbol cs
    in case sym of
         S.SymbolShort pre w  -> Right (wtab, cs', S.TShort cp pre w)
         S.SymbolCommon w     -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolGeneral w    -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolPlain w      -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolNumeric w    -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolUnknown w    -> Msg.forbiddenInput w

scanSpace :: Scan
scanSpace cp cs =
    let (cs', n) = S.nextSpace cs
    in Right (cs', S.TSpace cp $ n + 1)

scanSpace' :: Scan'
scanSpace' cp cs =
    let (cs', n) = S.nextSpace cs
    in (cs', S.TSpace cp $ n + 1)

scanQ :: ScanW
scanQ cp wtab cs =
    do (cs', w) <- S.nextSymbolPlain cs
       symbolToken S.TTextQ w cp wtab cs'

-- | Scan double-quoted text.
scanQQ :: Scan
scanQQ cp cs = do (cs', w) <- S.nextQQ cs
                  Right (cs', S.TTextQQ cp w)

-- | Scan slot name, like @aaa.
scanSlot :: Int -> Scan
scanSlot n cp cs =
    do (cs', w) <- S.nextSymbolPlain cs
       Right (cs', S.TSlot cp n w)

-- | Scan signed term name
scanTermSign :: Ordering -> ScanW
scanTermSign = scanTerm S.TermTypePath

-- | Scan term name
scanTermPath :: ScanW
scanTermPath = scanTerm S.TermTypePath EQ

scanTermPath' :: ScanW'
scanTermPath' = scanTerm' S.TermTypePath EQ

-- | Scan quoted term
scanTermQ :: ScanW
scanTermQ = scanTerm S.TermTypeQuoted EQ

scanTerm :: S.TermType -> Ordering -> ScanW
scanTerm q sign cp wtab = word [] where
    word ns (c:cs)
        | c == '='      = do (cs', w) <- S.nextSymbolPlain cs
                             nterm ns w cs'
        | isSymbol c    = do (cs', w) <- S.nextSymbolPlain (c:cs)
                             term (w : ns) cs'
        | isQQ c        = do (cs', w) <- S.nextQQ cs
                             term (w : ns) cs'
    word _ _            = Msg.expOrdSym

    nterm ns w cs'      = let n  = B.nioNumber $ B.codePtSource cp
                              w' = show n ++ ('=' : w)
                          in term (w' : ns) cs'

    term ns (c:cs) | isTerm c   = word ns cs
    term [n] cs | q == S.TermTypePath
                       = case Map.lookup n wtab of
                           Just n' -> Right (wtab, cs, S.TTermN cp sign n')
                           Nothing -> let wtab' = Map.insert n n wtab
                                      in Right (wtab', cs, S.TTermN cp sign n)
    term ns cs         = Right (wtab, cs, S.TTerm cp q $ rv ns)

-- Scan term name or term path
scanTerm' :: S.TermType -> Ordering -> ScanW'
scanTerm' q sign cp wtab cs0 = word [] cs0 where
    word ns ccs@(c:cs)
        | c == '='      = call (S.nextSymbolPlain cs)  (\w -> nterm ns w)
        | isSymbol c    = call (S.nextSymbolPlain ccs) (\w -> term (w : ns))
        | isQQ c        = call (S.nextQQ cs)           (\w -> term (w : ns))
    word _ _            = unk

    unk                 = (wtab, [], S.TUnknown cp cs0)
    call e f            = case e of
                            Right (cs', w) -> f w cs'
                            Left _         -> unk

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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokenizer of koshucode.

module Koshucode.Baala.Data.Token.TokenLine
  (
    -- * Library
    InputText,
    TokenLine,
    tokenLines,
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

import qualified Data.Map                             as Map
import qualified Data.Char                            as Ch
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Data.Token.AngleText as D
import qualified Koshucode.Baala.Data.Token.Next      as D
import qualified Koshucode.Baala.Data.Token.Short     as D
import qualified Koshucode.Baala.Data.Token.Token     as D
import qualified Koshucode.Baala.Base.Message         as Msg
import qualified Koshucode.Baala.Data.Token.Message   as Msg


-- ----------------------  Tokenizer

-- | Input data type
type InputText = String

-- | Token list on a line.
type TokenLine = B.CodeLine D.Token

-- | Code roll for token.
type TokenRoll = B.CodeRoll D.Token

-- | Split string into list of tokens.
--   Result token list does not contain newline characters.
tokens :: B.CodePiece -> InputText -> B.Ab [D.Token]
tokens res cs = do ls <- tokenLines res cs
                   Right $ concatMap B.lineTokens ls

-- | Abbreviated tokenizer.
toks :: InputText -> B.Ab [D.Token]
toks s = tokens (B.codeTextOf s) s

-- | Tokenize text.
tokenLines :: B.CodePiece -> InputText -> B.Ab [TokenLine]
tokenLines = B.codeRollUp relation

-- Line begins with the equal sign is treated as section delimter.
start :: (InputText -> B.Ab TokenRoll) -> B.CodePt -> TokenRoll -> B.Ab TokenRoll
start f cp r@B.CodeRoll { B.codeMap    = prev
                        , B.codeInput  = cs0
                        , B.codeOutput = out } = st out cs0 where
    st [] ('=' : _) = Right $ B.codeChange (section prev) r
    st _ cs         = Msg.abToken [cp] $ f cs

section :: B.AbMap TokenRoll -> B.AbMap TokenRoll
section prev r@B.CodeRoll { B.codeInputPt  = cp
                          , B.codeInput    = cs0
                          , B.codeWords    = ws
                          } = sec cs0 where
    v    = scan r
    vw   = scanW r
    out  = reverse $ D.sweepToken $ B.codeOutput r

    sec ""                    = dispatch out
    sec ('*' : '*' : _)       = dispatch out
    sec (c:cs)  | isSpace c   = v  $ scanSpace cp cs
                | isGeneral c = vw $ scanGeneral cp ws (c:cs)
                | otherwise   = Msg.unexpSect help

    dispatch :: [D.Token] -> B.Ab TokenRoll
    dispatch [D.TTextSect _] = Right $ B.codeChange prev r
    dispatch [D.TTextSect _, D.TTextRaw _ name] =
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

comment :: TokenRoll -> InputText -> B.Ab TokenRoll
comment r "" = Right r
comment r cs = Right $ B.codeUpdate "" tok r where
    tok  = D.TComment cp cs
    cp   = B.codeInputPt r

textLicense :: TokenRoll -> InputText -> B.Ab TokenRoll
textLicense r "" = Right r
textLicense r cs = Right $ B.codeUpdate "" tok r where
    tok  = D.TText cp D.TextLicense cs
    cp   = B.codeInputPt r


-- ----------------------  Relational section

-- | Split a next token from source text.
relation :: B.AbMap TokenRoll
relation r@B.CodeRoll { B.codeInputPt = cp, B.codeWords = ws } = r' where

    v              = scan r
    vw             = scanW r
    up             = u ""
    u   cs tok     = Right $ B.codeUpdate cs tok r
    int cs tok     = Right $ B.codeChange interp $ B.codeUpdate cs tok r

    sign '+'       = GT
    sign  _        = LT

    xs =^ pre      = pre `B.isPrefixOf` xs

    -- ----------------------  dispatch

    r' = start dispatch cp r

    dispatch ('(' : c : ')' : cs)
        | c `elem` "+-=#"        = u cs         $ D.TTextRaw cp ['(', c, ')']
    dispatch (a : b : cs)
        | a == '{' && b == '|'   = int cs       $ D.TOpen    cp [a,b]
        | isOpen a && isGrip b   = u cs         $ D.TOpen    cp [a,b]
        | isGrip a && isClose b  = u cs         $ D.TClose   cp [a,b]
        | (a == '+' || a == '-') && b == '/'
                                 = vw           $ scanTermSign (sign a) cp ws cs
        | a == '\'' && b == '/'  = vw           $ scanTermQ             cp ws cs
    dispatch ccs@(c : cs)
        | c == '*'            = aster cs [c]
        | c == '<'            = angle cs [c]
        | c == '@'            = at    cs 1
        | c == '|'            = bar   cs [c]
        | c == '^'            = hat   cs
        | ccs =^ "#!"         = up              $ D.TComment   cp cs
        | ccs =^ "-*-"        = up              $ D.TComment   cp cs
        | isOpen c            = u cs            $ D.TOpen      cp [c]
        | isClose c           = u cs            $ D.TClose     cp [c]
        | isSingle c          = u cs            $ D.TTextRaw   cp [c]
        | isTerm c            = vw              $ scanTermPath cp ws cs
        | isQQ c              = v               $ scanQQ       cp cs
        | isQ c               = vw              $ scanQ        cp ws cs
        | isShort c           = short cs [c]
        | isGeneral c         = vw              $ scanGeneral cp ws (c:cs)
        | isSpace c           = v               $ scanSpace   cp cs
        | otherwise           = Msg.forbiddenInput $ D.angleQuote [c]
    dispatch ""               = Right r

    short (c:cs) w
        | c == '.'            = let pre = rv w
                                    (cs', body) = D.nextGeneral cs
                                in u cs'        $ D.TShort   cp pre body
        | isGeneral c         = short cs (c:w)
    short cs w                = u cs            $ D.TTextRaw cp $ rv w

    -- ----------------------  begin with "@"

    at (c:cs) n | c == '@'    = at cs           $ n + 1
                | c == '\''   = v               $ scanSlot 0 cp cs  -- positional
    at cs n                   = v               $ scanSlot n cp cs

    -- ----------------------  begin with "*"

    aster (c:cs) w
        | w == "****"         = u (c:cs)        $ D.TTextRaw cp w
        | c == '*'            = aster cs (c:w)
    aster cs w
        | w == "**"           = up              $ D.TComment  cp cs
        | w == "***"          = up              $ D.TComment  cp cs
        | otherwise           = vw              $ scanGeneral cp ws $ w ++ cs

    -- ----------------------  begin with "^"

    -- read local reference, like ^/g
    hat ('/' : cs)                   = localToken cs D.LocalNest
    hat cs@(c : _) | isGeneral c     = localToken cs D.LocalSymbol
    hat _                            = Msg.adlib "local"

    localToken cs k                  = let (cs', w) = D.nextGeneral cs
                                       in u cs' $ D.TLocal cp (k w) (-1) []

    -- ----------------------  begin with "|"

    bar (c:cs) w
        | c == '|'                   = bar cs (c:w)
        | w == "|" && isJudge c      = judge cs [c, '|']
        | w == "|" && isGeneral c    = clock cs [c, '|']
    bar cs w                         = let cs' = B.trimLeft cs
                                       in u cs'        $ D.TTextRaw cp w

    -- read judgement sign, like |--, |-x
    judge (c:cs) w
        | isJudge c || Ch.isAlpha c  = judge cs (c:w)
        | isGeneral c                = clock (c:cs) w
    judge cs w                       = u cs            $ D.TTextBar cp $ rv w

    -- read clock, like |03:30|
    clock (c:cs) w | c == '|'        = u cs            $ D.TTextBar cp $ rv (c:w)
                   | isClock c       = clock cs (c:w)
    clock cs w                       = u cs            $ D.TTextBar cp $ rv w

    -- ----------------------  begin with "<"

    angle (c:cs) w | c == '<'        = angle cs (c:w)
    angle cs w     | w == "<"        = angleMid cs ""
                   | otherwise       = u cs            $ D.TTextRaw cp w

    -- read keyword, like <crlf>
    angleMid (c:cs) w  | c == '>'    = u cs            $ angleToken $ rv w
                       | isGeneral c = angleMid cs (c:w)
    angleMid cs w                    = u cs            $ D.TTextRaw cp $ '<' : rv w

    angleToken ""                    = D.TTextRaw cp "<>"
    angleToken ('c' : s)
        | isCharCode s  = case charCodes s of
                            Just ns  -> D.TTextKey cp $ map Ch.chr ns
                            Nothing  -> D.TTextUnk cp s
    angleToken s        = case lookup s D.angleTexts of
                            Just w   -> D.TTextKey cp w
                            Nothing  -> D.TTextUnk cp s

charCodes :: InputText -> Maybe [Int]
charCodes = mapM B.readInt . B.omit null . B.divide '-'

-- interpretation content between {| and |}
interp :: B.AbMap TokenRoll
interp r@B.CodeRoll { B.codeInputPt = cp, B.codeWords = ws } = start int cp r where

    v           = scan r
    vw          = scanW r
    u   cs tok  = Right $ B.codeUpdate cs tok r
    gen cs tok  = Right $ B.codeChange relation $ B.codeUpdate cs tok r

    int ""                           = Right r
    int (c:cs)    | isSpace c        = v         $ scanSpace    cp cs
                  | isTerm c         = vw        $ scanTermPath cp ws cs
                  | otherwise        = word (c:cs) ""

    word cs@('|':'}':_) w            = gen cs    $ D.TTextRaw cp $ rv w
    word (c:cs) w | isSpace c        = u (c:cs)  $ D.TTextRaw cp $ rv w
                  | isTerm c         = u (c:cs)  $ D.TTextRaw cp $ rv w
                  | otherwise        = word cs   $ c:w
    word cs w                        = u cs      $ D.TTextRaw cp $ rv w


-- ----------------------  Scanner

type Scan  = B.CodePt -> InputText -> B.Ab (InputText, D.Token)
type ScanW = B.CodePt -> B.WordTable -> InputText -> B.Ab (B.WordTable, InputText, D.Token)

rv :: B.Map [a]
rv = reverse

scan :: TokenRoll -> B.Ab (InputText, D.Token) -> B.Ab TokenRoll
scan r (Right (cs, tok)) = Right $ B.codeUpdate cs tok r
scan _ (Left message)    = Left message

-- Scan with word table.
scanW :: TokenRoll -> B.Ab (B.WordTable, InputText, D.Token) -> B.Ab TokenRoll
scanW r (Right (wtab, cs, tok))  = Right $ B.codeUpdateWords wtab cs tok r
scanW _ (Left message)           = Left message

scanSpace :: Scan
scanSpace cp cs =
    let (cs', n) = D.nextSpace cs
    in Right (cs', D.TSpace cp $ n + 1)

scanGeneral :: ScanW
scanGeneral cp wtab cs =
    let (cs', w) = D.nextGeneral cs
    in case Map.lookup w wtab of
         Just w' -> Right (wtab, cs', D.TTextRaw cp w')
         Nothing -> let wtab' = Map.insert w w wtab
                    in Right (wtab', cs', D.TTextRaw cp w)

scanQ :: ScanW
scanQ cp wtab cs =
    let (cs', w) = D.nextGeneral cs
    in case Map.lookup w wtab of
         Just w' -> Right (wtab, cs', D.TTextQ cp w')
         Nothing -> let wtab' = Map.insert w w wtab
                    in Right (wtab', cs', D.TTextQ cp w)

-- | Scan double-quoted text.
scanQQ :: Scan
scanQQ cp cs = do (cs', w) <- D.nextQQ cs
                  Right (cs', D.TTextQQ cp w)

-- | Scan slot name, like @aaa.
scanSlot :: Int -> Scan
scanSlot n cp cs = case D.nextSymbol cs of
                     (cs', D.SymbolCommon   w) -> Right (cs', D.TSlot cp n w)
                     (cs', D.SymbolOrdinary w) -> Right (cs', D.TSlot cp n w)
                     _ -> Msg.expOrdSym

-- | Scan signed term name
scanTermSign :: Ordering -> ScanW
scanTermSign = scanTerm D.TermTypePath

-- | Scan term name
scanTermPath :: ScanW
scanTermPath = scanTerm D.TermTypePath EQ

-- | Scan quoted term
scanTermQ :: ScanW
scanTermQ = scanTerm D.TermTypeQuoted EQ

scanTerm :: D.TermType -> Ordering -> ScanW
scanTerm q sign cp wtab = word [] where
    word ns (c:cs)
        | c == '='      = case D.nextSymbol cs of
                            (cs', D.SymbolCommon   w) -> nterm ns w cs'
                            (cs', D.SymbolOrdinary w) -> nterm ns w cs'
                            _ -> Msg.expOrdSym
        | D.isSymbol c  = case D.nextSymbol (c:cs) of
                            (cs', D.SymbolCommon   w) -> term (w : ns) cs'
                            (cs', D.SymbolOrdinary w) -> term (w : ns) cs'
                            _ -> Msg.expOrdSym
        | isQQ c        = do (cs', w) <- D.nextQQ cs
                             term (w : ns) cs'
    word _ _            = Msg.expOrdSym

    nterm ns w cs'      = let n  = B.codeNumber $ B.codePtSource cp
                              w' = show n ++ ('=' : w)
                          in term (w' : ns) cs'

    term ns (c:cs) | isTerm c   = word ns cs
    term [n] cs | q == D.TermTypePath
                       = case Map.lookup n wtab of
                           Just n' -> Right (wtab, cs, D.TTermN cp sign n')
                           Nothing -> let wtab' = Map.insert n n wtab
                                      in Right (wtab', cs, D.TTermN cp sign n)
    term ns cs         = Right (wtab, cs, D.TTerm cp q $ rv ns)


-- ----------------------  Char category

-- Punctuations
isOpen, isClose, isGrip, isJudge, isSingle, isQ, isQQ, isTerm, isSpace, isGeneral :: B.Pred Char
isOpen     = ( `elem` "([{"    )  -- Punctuation
isClose    = ( `elem` "}])"    )  -- Punctuation
isGrip     = ( `elem` "-=|?"   )  -- Punctuation | Symbol   -- :*+
isJudge    = ( `elem` "-="     )  -- Punctuation | Symbol
isSingle   = ( `elem` ":|"     )  -- Punctuation | Symbol
isQ        = (    ==  '\''     )  -- Punctuation
isQQ       = (    ==  '"'      )  -- Punctuation
isTerm     = (    ==  '/'      )  -- Punctuation
isSpace    = Ch.isSpace
isGeneral  = D.isGeneralChar

isShortPrefix :: B.Pred String
isShortPrefix = all isShort

isShort :: B.Pred Char
isShort = Ch.isAlpha

isCharCode :: B.Pred String
isCharCode = all isFigure

isFigure :: B.Pred Char
isFigure '-' = True
isFigure c   = Ch.isDigit c

isClock :: B.Pred Char
isClock c = Ch.isDigit c || c `elem` ".:'+-"


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

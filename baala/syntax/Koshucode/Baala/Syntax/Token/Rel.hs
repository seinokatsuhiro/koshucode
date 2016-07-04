{-# OPTIONS_GHC -Wall #-}

-- | Tokenizer for relational section.

module Koshucode.Baala.Syntax.Token.Rel
  ( -- * Section
    sectionRel,
  
    -- * Token type
    -- $TokenType
  
    -- * Asterisks
    -- $Asterisks
  ) where

import qualified Data.Map                               as Map
import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Nipper    as S
import qualified Koshucode.Baala.Syntax.Token.Pattern   as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Token.Section   as S
import qualified Koshucode.Baala.Base.Message           as Msg
import qualified Koshucode.Baala.Syntax.Symbol.Message  as Msg
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


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
sectionRel change sc@B.CodeScan { B.codeInputPt = cp, B.codeWords = wtab } = r' where

    v              = S.scan  sc
    vw             = S.scanW sc
    up             = u ""
    u   cs tok     = B.codeUpdate cs tok sc
    int cs tok     = B.codeChange (interp change) $ B.codeUpdate cs tok sc

    sign '+'       = GT
    sign  _        = LT

    -- ----------------------  dispatch

    r' = S.section change (uncons3 '\0' dispatch) sc

    dispatch n a b c bs cs ds
        | S.isSpace a            = v               $ S.nipSpace  cp bs
        | isTerm a               = vw              $ nipTermPath cp wtab bs
        | isPM a && isTerm b     = vw              $ nipTermSign (sign a) cp wtab cs
        | isQQ a                 = v               $ nipQQ       cp bs
        | isQ a && isTerm b      = vw              $ nipTermQ    cp wtab cs
        | isQ a                  = vw              $ nipQ        cp wtab bs

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
        | S.isSymbol a           = vw              $ S.nipSymbol cp wtab $ a : bs
        | n == 0                 = sc
        | otherwise              = u []            $ S.unknownToken cp cs
                                                   $ Msg.forbiddenInput $ S.angleQuote [a]

    -- ----------------------  begin with "@"

    at (c:cs) n | c == '@'    = at cs           $ n + 1
                | c == '\''   = v               $ nipSlot 0 cp cs  -- positional
    at cs n                   = v               $ nipSlot n cp cs

    -- ----------------------  begin with "*"

    aster (c:cs) w
        | w == "****"         = u (c:cs)        $ S.TTextRaw cp w
        | c == '*'            = aster cs (c:w)
    aster cs w
        | w == "**"           = up              $ S.TComment cp cs
        | w == "***"          = up              $ S.TComment cp cs
        | otherwise           = vw              $ S.nipSymbol cp wtab $ w ++ cs

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
interp change sc@B.CodeScan { B.codeInputPt = cp
                            , B.codeWords = wtab } = S.section change int sc where

    v           = S.scan  sc
    vw          = S.scanW sc
    u   cs tok  = B.codeUpdate cs tok sc
    gen cs tok  = B.codeChange (sectionRel change) $ B.codeUpdate cs tok sc

    int ""                           = sc
    int (c:cs)    | S.isSpace c      = v         $ S.nipSpace   cp cs
                  | isTerm c         = vw        $ nipTermPath  cp wtab cs
                  | otherwise        = word (c:cs) ""

    word cs@('|':'}':_) w            = gen cs    $ S.TTextRaw cp $ rv w
    word (c:cs) w | S.isSpace c      = u (c:cs)  $ S.TTextRaw cp $ rv w
                  | isTerm c         = u (c:cs)  $ S.TTextRaw cp $ rv w
                  | otherwise        = word cs   $ c:w
    word cs w                        = u cs      $ S.TTextRaw cp $ rv w


-- ----------------------  Nipper

rv :: B.Map [a]
rv = reverse

-- | Nip off a single-quoted text.
nipQ :: S.TokenNipW
nipQ cp wtab cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> S.symbolToken S.TTextQ w cp wtab cs'
      Left a         -> (wtab, [], S.TUnknown cp cs a)
          

-- | Nip off a double-quoted text.
nipQQ :: S.TokenNip
nipQQ cp cs = case S.nextQQ cs of
                Right (cs', w) -> (cs', S.TTextQQ cp w)
                Left a         -> ([], S.TUnknown cp cs a)

-- | Nip off a slot name, like @aaa.
nipSlot :: Int -> S.TokenNip
nipSlot n cp cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> (cs', S.TSlot cp n w)
      Left a         -> ([], S.TUnknown cp cs a)
      

-- | Nip off a signed term name.
nipTermSign :: Ordering -> S.TokenNipW
nipTermSign = nipTerm S.TermTypePath

-- | Nip off a term name.
nipTermPath :: S.TokenNipW
nipTermPath = nipTerm S.TermTypePath EQ

-- | Nip off a quoted term.
nipTermQ :: S.TokenNipW
nipTermQ = nipTerm S.TermTypeQuoted EQ

-- | Nip off a term name or a term path.
nipTerm :: S.TermType -> Ordering -> S.TokenNipW
nipTerm q sign cp wtab cs0 = word [] cs0 where
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

isCharCode :: B.Pred String
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


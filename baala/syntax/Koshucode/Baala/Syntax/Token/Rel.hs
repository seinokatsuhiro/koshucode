{-# OPTIONS_GHC -Wall #-}

-- | Tokenizer for relational section.

module Koshucode.Baala.Syntax.Token.Rel
  ( -- * Section
    scanRel,
    scanInterp,
  
    -- * Token type
    -- $TokenType
  
    -- * Asterisks
    -- $Asterisks
  ) where

import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Nipper    as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Token.Section   as S
import qualified Koshucode.Baala.Base.Message           as Msg
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- ----------------------  Utility

uncons3 :: a -> (Int -> a -> a -> a -> [a] -> [a] -> [a] -> b) -> [a] -> b
uncons3 z f = first where
    first (a:bs)            = second a bs
    first []                = f 0 z z z [] [] []

    second a bs@(b:cs)      = third a b bs cs
    second a []             = f 1 a z z [] [] []

    third a b bs cs@(c:ds)  = f 3 a b c bs cs ds
    third a b bs []         = f 2 a b z bs [] []

-- | Convert decimal string to integer.
--
--   >>> stringIntList "50"
--   Just [50]
--
--   >>> stringIntList "50-51"
--   Just [50, 51]
--
--   >>> stringIntList "A"
--   Nothing
--
stringIntList :: S.InputText -> Maybe [Int]
stringIntList = mapM O.stringInt . B.omit null . B.divide '-'

-- | Convert hexadecimal string to integer.
--
--   >>> stringHexIntList "20+21"
--   Just [32,33]
--
stringHexIntList :: S.InputText -> Maybe [Int]
stringHexIntList = mapM O.stringHexInt . B.omit null . B.divide '+'

rv :: O.Map [a]
rv = reverse

-- Punctuations
isOpen, isClose, isGrip, isSingle, isQ, isPM :: O.Test Char
isOpen     = ( `elem` "([{"    )  -- Punctuation
isClose    = ( `elem` "}])"    )  -- Punctuation
isGrip     = ( `elem` "-=|?"   )  -- Punctuation | Symbol   -- :*+
isSingle   = ( `elem` ":|#"    )  -- Punctuation | Symbol
isQ        = (    ==  '\''     )  -- Punctuation
isPM a     = (a == '+' || a == '-')

isCodePoint :: O.Test String
isCodePoint = all isCodePointDigit

isCodePointDigit :: O.Test Char
isCodePointDigit c = isPM c || Ch.isDigit c


-- ----------------------  Relational section

-- | Scan a next token in relational section.
scanRel :: S.Scanner
scanRel change sc@B.CodeScan { B.codeInputPt = cp, B.codeWords = wtab } = sc' where

    nip          = S.nipUpdate   sc
    nipw         = S.nipUpdateW  sc
    niplw        = S.nipUpdateLW sc
    upd cs tok   = B.codeUpdate cs tok sc
    updEnd       = upd ""
    int cs tok   = B.codeChange (scanInterp change) $ B.codeScanSave $ upd cs tok
    raw          = S.TText cp S.TextRaw

    -- ----------------------  dispatch

    sc' = S.section change (uncons3 '\0' dispatch) sc

    dispatch n a b c bs cs ds
        | S.isSpace a            = nip             $ S.nipSpace    cp bs
        | S.isTerm a             = niplw           $ S.nipTermName cp wtab bs
        | isPM a && S.isTerm b   = niplw           $ S.nipTermSign [a,b] cp wtab cs
        | S.isQQ a               = nipw            $ S.nipQQ       cp wtab bs
        | isQ a && S.isTerm b    = niplw           $ S.nipTermQ    cp wtab cs
        | isQ a                  = nipw            $ S.nipQ        cp wtab bs

        | a == '(' && c == ')' && b `elem` "+-/=#"
                                 = upd ds          $ raw             [a,b,c]
        | a == '{' && b == '|'   = int cs          $ S.TOpen      cp [a,b]
        | isOpen a && isGrip b   = upd cs          $ S.TOpen      cp [a,b]
        | isGrip a && isClose b  = upd cs          $ S.TClose     cp [a,b]
        | isOpen a               = upd bs          $ S.TOpen      cp [a]
        | isClose a              = upd bs          $ S.TClose     cp [a]

        | a == '*'               = aster bs [a]
        | a == '<'               = nip $ nipAngle cp bs [a]
        | a == '@'               = nip $ nipAt    cp bs 1
        | a == '|'               = nip $ S.nipBar cp bs [a]
        | a == '^'               = nip $ nipHat   cp bs
        | a == '#' && b == '!'   = updEnd          $ S.TComment   cp bs
        | a == '-' && b == '*' && c == '-'
                                 = updEnd          $ S.TComment   cp bs

        | isSingle a             = upd bs          $ raw [a]
        | S.isSymbol a           = nipw            $ S.nipSymbol cp wtab $ a : bs
        | n == 0                 = sc
        | otherwise              = upd []          $ S.unknownToken cp cs
                                                   $ Msg.forbiddenInput $ S.angleQuote [a]

    aster :: String -> String -> S.TokenScan
    aster (c:cs) w
        | w == "****"         = upd (c:cs)      $ raw w
        | c == '*'            = aster cs (c:w)
    aster cs w
        | w == "**"           = updEnd          $ S.TComment cp cs
        | w == "***"          = updEnd          $ S.TComment cp cs
        | otherwise           = nipw            $ S.nipSymbol cp wtab $ w ++ cs

-- | Nip off token beginning with @<@.
nipAngle :: B.CodePos -> String -> String -> S.TokenNipResult
nipAngle cp = angle where
    raw = S.TText cp S.TextRaw

    angle (c:cs) w | c == '<'        = angle cs (c:w)
    angle cs w     | w == "<"        = angleMid cs ""
                   | otherwise       = (cs, raw w)

    -- read keyword, like <crlf>
    angleMid (c:cs) w
        | c == '>'                   = (cs, angleToken $ rv w)
        | S.isSymbol c               = angleMid cs (c:w)
    angleMid cs w                    = (cs, raw $ '<' : rv w)

    angleToken ""                = raw "<>"
    angleToken ('U' : '+' : s)   = fromCodePoint stringHexIntList s
    angleToken ('c' : s) | isCodePoint s
                                 = fromCodePoint stringIntList s
    angleToken s                 = case lookup s S.angleTexts of
                                     Just w   -> S.TText cp S.TextKey w
                                     Nothing  -> S.TText cp S.TextUnk s

    fromCodePoint f s = case f s of
                          Just ns  -> S.TText cp S.TextKey (toEnum <$> ns)
                          Nothing  -> S.TText cp S.TextUnk s

-- | Nip off token beginning with "@".
nipAt :: B.CodePos -> String -> Int -> S.TokenNipResult
nipAt cp = at where
    at (c:cs) n | c == '@'           = at cs $ n + 1
                | c == '\''          = S.nipSlot 0 cp cs  -- positional
    at cs n                          = S.nipSlot n cp cs

-- | Nip off local reference token, like @^/g@.
nipHat :: B.CodePos -> String -> S.TokenNipResult
nipHat cp = hat where
    hat ('/' : cs)                   = localToken cs (S.LocalNest . S.toTermName)
    hat cs@(c : _) | S.isSymbol c    = localToken cs S.LocalSymbol
    hat cs                           = ([], S.unknownToken cp cs $ Msg.adlib "local")

    localToken cs k                  = case S.nextSymbolPlain cs of
                                         Right (cs', w) -> (cs', S.TLocal cp (k w) (-1) [])
                                         Left a         -> ([],  S.TUnknown cp cs a)


-- ----------------------  Interpretation

-- | Scan interpretation content between @{|@ and @|}@.
scanInterp :: S.Scanner
scanInterp change sc@B.CodeScan { B.codeInputPt = cp
                                , B.codeWords = wtab } = S.section change int sc where
    nip         = S.nipUpdate   sc
    niplw       = S.nipUpdateLW sc
    upd cs tok  = B.codeUpdate cs tok sc
    gen cs tok  = B.codeScanRestore $ upd cs tok
    raw         = S.TText cp S.TextRaw

    int ""                           = sc
    int (c:cs)    | S.isSpace c      = nip         $ S.nipSpace    cp cs
                  | S.isTerm c       = niplw       $ S.nipTermName cp wtab cs
                  | otherwise        = word (c:cs) ""

    word cs@('|':'}':_) w            = gen cs      $ raw $ rv w
    word (c:cs) w | S.isSpace c      = upd (c:cs)  $ raw $ rv w
                  | S.isTerm c       = upd (c:cs)  $ raw $ rv w
                  | otherwise        = word cs     $ c:w
    word cs w                        = upd cs      $ raw $ rv w


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


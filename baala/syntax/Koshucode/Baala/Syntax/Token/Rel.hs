{-# LANGUAGE ViewPatterns #-}
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
import qualified Koshucode.Baala.Syntax.Token.Clip      as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Token.Section   as S
import qualified Koshucode.Baala.Base.Message           as Msg
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- ----------------------  Utility

tCut3 :: (O.Textual t) => Char -> (Int -> Char -> Char -> Char -> t -> t -> t -> a) -> t -> a
tCut3 z f = first where
    first (O.tCut -> Just (a, bs))
                     = second a bs
    first _          = f 0 z z z O.tEmpty O.tEmpty O.tEmpty

    second a bs@(O.tCut -> Just (b, cs))
                     = third a b bs cs
    second a _       = f 1 a z z O.tEmpty O.tEmpty O.tEmpty

    third a b bs cs@(O.tCut -> Just (c, ds))
                     = f 3 a b c bs cs ds
    third a b bs _   = f 2 a b z bs O.tEmpty O.tEmpty

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

    clip         = S.clipUpdate   sc
    clipw        = S.clipUpdateC  sc
    clipcl       = S.clipUpdateCL sc
    upd cs tok   = B.codeUpdate cs tok sc
    updEnd       = upd O.tEmpty
    int cs tok   = B.codeChange (scanInterp change) $ B.codeScanSave $ upd cs tok
    raw          = S.TText cp S.TextRaw

    -- ----------------------  dispatch

    sc' = S.section change (tCut3 '\0' dispatch) sc

    dispatch n a b c bs cs ds
        | S.isSpace a            = clip    $ S.clipSpace    cp bs
        | S.isTerm a             = clipcl  $ S.clipTermName cp wtab bs
        | isPM a && S.isTerm b   = clipcl  $ S.clipTermSign (O.char2T a b) cp wtab cs
        | S.isQQ a               = clipw   $ S.clipQQ       cp wtab bs
        | isQ a && S.isTerm b    = clipcl  $ S.clipTermQ    cp wtab cs
        | isQ a                  = clipw   $ S.clipQ        cp wtab bs

        | a == '(' && c == ')' && b `elem` "+-/=#"
                                 = upd ds  $ raw             (O.char3T a b c)
        | a == '{' && b == '|'   = int cs  $ S.TOpen      cp (O.char2T a b)
        | isOpen a && isGrip b   = upd cs  $ S.TOpen      cp (O.char2T a b)
        | isGrip a && isClose b  = upd cs  $ S.TClose     cp (O.char2T a b)
        | isOpen a               = upd bs  $ S.TOpen      cp (O.charT a)
        | isClose a              = upd bs  $ S.TClose     cp (O.charT a)

        | a == '*'               = aster bs (O.charT a)
        | a == '<'               = clip    $ clipAngle cp bs
        | a == '@'               = clip    $ clipAt    cp bs 1
        | a == '|'               = clip    $ S.clipBar cp bs
        | a == '^'               = clip    $ clipHat   cp bs
        | a == '#' && b == '!'   = updEnd  $ S.TComment   cp bs
        | a == '-' && b == '*' && c == '-'
                                 = updEnd  $ S.TComment   cp bs

        | isSingle a             = upd bs  $ raw (O.charT a)
        | S.isSymbol a           = clipw   $ S.clipSymbol cp wtab $ O.tAdd a bs
        | n == 0                 = sc
        | otherwise              = upd []  $ S.unknownToken cp cs
                                           $ Msg.forbiddenInput $ S.angleQuote (O.charT a)

    aster :: String -> String -> S.TokenScan
    aster (O.tCut -> Just (c, cs)) w
        | w == "****"            = upd (O.tAdd c cs) $ raw w
        | c == '*'               = aster cs (O.tAdd c w)
    aster cs w
        | w == "**"              = updEnd  $ S.TComment cp cs
        | w == "***"             = updEnd  $ S.TComment cp cs
        | otherwise              = clipw   $ S.clipSymbol cp wtab $ w ++ cs

-- | Clip token beginning with @'<'@.
--
--   >>> clipAngle B.def "crlf> ..."
--   (" ...", TText /0.0.0/ TextKey "\r\n")
--
--   >>> clipAngle B.def "> 0"
--   (" 0", TText /0.0.0/ TextRaw "<>")
--
--   >>> clipAngle B.def "<< 0"
--   (" 0",TText /0.0.0/ TextRaw "<<<")
--
--   >>> clipAngle B.def "U+4B> ..."
--   (" ...", TText /0.0.0/ TextKey "K")
--
clipAngle :: B.CodePos -> String -> S.ClipResult
clipAngle cp cs0 = angle (0 :: Int) cs0 where
    raw = S.TText cp S.TextRaw
    text n = O.tTake n cs0

    -- <...
    angle :: Int -> String -> S.ClipResult
    angle n (O.tCut -> Just (c, cs))
        | S.isSymbol c  = sym n (c:cs)
    angle n cs          = (cs, raw $ O.tAdd '<' $ text n)

    -- <symbol ...
    sym :: Int -> String -> S.ClipResult
    sym n (O.tCut -> Just (c, cs))
        | c == '>'      = close n cs
        | S.isSymbol c  = sym (n + 1) cs
    sym n cs            = (cs, raw $ O.tAdd '<' $ text n) -- '<<'

    -- <symbol> ...
    close :: Int -> String -> S.ClipResult
    close n (O.tCut -> Just (c, cs))
        | c == '>'    = close (n + 1) cs
    close n cs        = (cs, key $ text n)

    -- symbol in '<symbol>'
    key :: String -> S.Token
    key (O.tCut2 -> Just ('U', Just ('+', s))) = fromCodePoint stringHexIntList s
    key (O.tCut  -> Just ('c', s))
          | isCodePoint s  = fromCodePoint stringIntList s -- obsolete
    key s | O.tIsEmpty s   = raw "<>"
          | otherwise      = case lookup s S.angleTexts of
                               Just w  -> S.TText cp S.TextKey w
                               Nothing -> S.TText cp S.TextUnk s

    fromCodePoint f s = case f s of
                          Just ns  -> S.TText cp S.TextKey (toEnum <$> ns)
                          Nothing  -> S.TText cp S.TextUnk s

-- | Clip token beginning with "@".
clipAt :: B.CodePos -> String -> Int -> S.ClipResult
clipAt cp = at where
    at (O.tCut -> Just (c, cs))
       n | c == '@'    = at cs $ n + 1
         | c == '\''   = S.clipSlot 0 cp cs  -- positional
    at cs n            = S.clipSlot n cp cs

-- | Clip local reference token, like @^/g@.
clipHat :: B.CodePos -> String -> S.ClipResult
clipHat cp = hat where
    hat (O.tCut -> Just ('/', cs)) = localToken cs (S.LocalNest . S.toTermName)
    hat cs@(O.tCut -> Just (c, _))
        | S.isSymbol c             = localToken cs S.LocalSymbol
    hat cs                         = ([], S.unknownToken cp cs $ Msg.adlib "local")

    localToken cs k                = case S.nextSymbolPlain cs of
                                       Right (cs', w) -> (cs', S.TLocal cp (k w) (-1) [])
                                       Left a         -> ([],  S.TUnknown cp cs a)


-- ----------------------  Interpretation

-- | Scan interpretation content between @{|@ and @|}@.
scanInterp :: S.Scanner
scanInterp change sc@B.CodeScan { B.codeInputPt = cp
                                , B.codeWords = wtab } = S.section change int sc where
    clip        = S.clipUpdate   sc
    clipcl      = S.clipUpdateCL sc
    upd cs tok  = B.codeUpdate cs tok sc
    gen cs tok  = B.codeScanRestore $ upd cs tok

    int (O.tCut -> Just (c, cs))
        | S.isSpace c   = clip   $ S.clipSpace    cp cs
        | S.isTerm c    = clipcl $ S.clipTermName cp wtab cs
        | otherwise     = word (c:cs)
    int _               = sc

    word cs0 = loop O.zero cs0 where
        raw n = S.TText cp S.TextRaw $ O.tTake n cs0
        loop n cs@(O.tCut2 -> Just ('|', Just ('}', _)))
                            = gen  cs            $ raw n
        loop n (O.tCut -> Just (c, cs))
            | S.isSpace c   = upd  (O.tAdd c cs) $ raw n
            | S.isTerm c    = upd  (O.tAdd c cs) $ raw n
            | otherwise     = loop (n + 1) cs
        loop n cs           = upd  cs            $ raw n


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


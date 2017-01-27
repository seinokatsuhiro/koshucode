{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokenizer for relational section.

module Koshucode.Baala.Syntax.Token.Rel
  ( scanRel,
    scanInterp,
  ) where

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
    first (O.tCut -> O.Jp a bs)
                     = second a bs
    first _          = f 0 z z z O.tEmpty O.tEmpty O.tEmpty

    second a bs@(O.tCut -> O.Jp b cs)
                     = third a b bs cs
    second a _       = f 1 a z z O.tEmpty O.tEmpty O.tEmpty

    third a b bs cs@(O.tCut -> O.Jp c ds)
                     = f 3 a b c bs cs ds
    third a b bs _   = f 2 a b z bs O.tEmpty O.tEmpty

-- | Convert hexadecimal text to integers.
--
--   >>> textHexInts "20+21"
--   Just [32,33]
--
textHexInts :: (O.Textual t) => t -> Maybe [Int]
textHexInts = mapM O.stringHexInt . B.omit O.tIsEmpty . O.tDivide (== '+')

-- Punctuations
isOpen, isClose, isGrip, isSingle, isQ, isPM :: O.Test Char
isOpen     = ( `elem` "([{"    )  -- Punctuation
isClose    = ( `elem` "}])"    )  -- Punctuation
isGrip     = ( `elem` "-=|?"   )  -- Punctuation | Symbol   -- :*+
isSingle   = ( `elem` ":|#"    )  -- Punctuation | Symbol
isQ        = (    ==  '\''     )  -- Punctuation
isPM a     = (a == '+' || a == '-')


-- ----------------------  Relational section

-- | Scan a next token in relational section.
scanRel :: (S.TextualTermName t) => S.Scanner t
scanRel change sc@B.CodeScan { B.scanCp = cp, B.scanCache = wtab } = sc' where

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
        | S.isQQ a               = clipw   $ S.clipQq       cp wtab bs
        | isQ a && S.isTerm b    = clipcl  $ S.clipTermQ    cp wtab cs
        | isQ a && isQ b         = clipw   $ S.clipQn       cp wtab cs
        | isQ a                  = clipw   $ S.clipQ        cp wtab bs

        | a == '(' && c == ')' && b `elem` "+-/=#"
                                 = upd ds  $ raw              (O.char3T a b c)
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
        | a == '#' && b == '!'   = updEnd  $ S.TComment cp bs
        | a == '-' && b == '*' && c == '-'
                                 = updEnd  $ S.TComment cp bs

        | isSingle a             = upd bs  $ raw (O.charT a)
        | S.isSymbol a           = clipw   $ S.clipSymbol cp wtab $ O.tAdd a bs
        | n == 0                 = sc
        | otherwise              = upd O.tEmpty $ S.unknownToken cp cs
                                                $ Msg.forbiddenInput $ S.angleQuote [a]

    aster (O.tCut -> O.Jp c cs) w
        | w == O.stringT "****"  = upd (O.tAdd c cs) $ raw w
        | c == '*'               = aster cs (O.tAdd c w)
    aster cs w
        | w == O.stringT "**"    = updEnd  $ S.TComment cp cs
        | w == O.stringT "***"   = updEnd  $ S.TComment cp cs
        | otherwise              = clipw   $ S.clipSymbol cp wtab $ w O.++ cs

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
clipAngle :: (O.Textual t) => B.TCodePos t -> t -> S.ClipResult t
clipAngle cp cs0 = angle (0 :: Int) cs0 where
    raw = S.TText cp S.TextRaw
    text n = O.tTake n cs0

    -- <...
    angle n (O.tCut -> O.Jp c cs)
        | S.isSymbol c  = sym n (O.tAdd c cs)
    angle n cs          = (cs, raw $ O.tAdd '<' $ text n)

    -- <symbol ...
    sym n (O.tCut -> O.Jp c cs)
        | c == '>'      = close n cs
        | S.isSymbol c  = sym (n + 1) cs
    sym n cs            = (cs, raw $ O.tAdd '<' $ text n) -- '<<'

    -- <symbol> ...
    close n (O.tCut -> O.Jp c cs)
        | c == '>'    = close (n + 1) cs
    close n cs        = (cs, key $ text n)

    -- symbol in '<symbol>'
    key (O.tCut2 -> O.Jp2 'U' '+' s) = fromCodePoint textHexInts s
    key s | O.tIsEmpty s   = raw $ O.stringT "<>"
          | otherwise      = case lookup s S.angleTexts of
                               Just w  -> S.TText cp S.TextKey w
                               Nothing -> S.TText cp S.TextUnk s
    fromCodePoint f s =
        case f s of
          Just ns  -> S.TText cp S.TextKey $ tFromCode ns
          Nothing  -> S.TText cp S.TextUnk s

tFromCode :: (O.Textual t) => [Int] -> t
tFromCode ns = O.stringT (toEnum <$> ns)

-- | Clip token beginning with "@".
clipAt :: (O.Textual t) => B.TCodePos t -> t -> Int -> S.ClipResult t
clipAt cp = at where
    at (O.tCut -> O.Jp c cs)
       n | c == '@'    = at cs $ n + 1
         | c == '\''   = S.clipSlot 0 cp cs  -- positional
    at cs n            = S.clipSlot n cp cs

-- | Clip local reference token, like @^/g@.
clipHat :: (S.TextualTermName t) => B.TCodePos t -> t -> S.ClipResult t
clipHat cp = hat where
    hat (O.tCut -> O.Jp '/' cs) = localToken cs (S.LocalNest . S.toTermName)
    hat cs@(O.tCut -> O.Jp c _)
        | S.isSymbol c          = localToken cs (S.LocalSymbol . O.tString)
    hat cs                      = (O.tEmpty, S.unknownToken cp cs $ Msg.adlib "local")

    localToken cs k = case S.nextSymbolPlain cs of
                        Right (cs', w) -> (cs', S.TLocal cp (k w) (-1) [])
                        Left a         -> (O.tEmpty, S.TUnknown cp cs a)


-- ----------------------  Interpretation

-- | Scan interpretation content between @{|@ and @|}@.
scanInterp :: (O.Textual t) => S.Scanner t
scanInterp change sc@B.CodeScan { B.scanCp = cp
                                , B.scanCache = wtab } = S.section change int sc where
    clip        = S.clipUpdate   sc
    clipcl      = S.clipUpdateCL sc
    upd cs tok  = B.codeUpdate cs tok sc
    gen cs tok  = B.codeScanRestore $ upd cs tok

    int (O.tCut -> O.Jp c cs)
        | S.isSpace c   = clip   $ S.clipSpace    cp cs
        | S.isTerm c    = clipcl $ S.clipTermName cp wtab cs
        | otherwise     = word (O.tAdd c cs)
    int _               = sc

    word cs0 = loop O.zero cs0 where
        raw n = S.TText cp S.TextRaw $ O.tTake n cs0
        loop n cs@(O.tCut2 -> O.Jp2 '|' '}' _)
                            = gen  cs            $ raw n
        loop n (O.tCut -> O.Jp c cs)
            | S.isSpace c   = upd  (O.tAdd c cs) $ raw n
            | S.isTerm c    = upd  (O.tAdd c cs) $ raw n
            | otherwise     = loop (n + 1) cs
        loop n cs           = upd  cs            $ raw n


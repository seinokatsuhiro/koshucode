{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Clip token text.

module Koshucode.Baala.Syntax.Token.Clip
  ( -- * Char test
    isSymbol,
    isSpace,
    isQQ,
    isTerm,
    isJudge,
    isClock,

    -- * Type
    TokenScan, TokenScanMap,
    ClipResult, ClipResultC, ClipResultCL,
    ClipToken, ClipTokenC, ClipTokenCL,

    -- * Clip
    -- ** Updater
    clipUpdate, clipUpdateC, clipUpdateCL,
    -- ** Textual
    clipSpace,
    clipQ, clipQq, clipQn,
    -- ** Identifier
    clipSymbol,
    --symbolToken,
    clipSlot,
    -- ** Term name
    clipTermSign,
    clipTermName,
    clipTermQ,
    -- ** Symbol
    clipBar,
  ) where

import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Symbol.Message  as Msg
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- ============================================  Char test

-- | Test character is symbolic.
isSymbol :: O.Test Char
isSymbol = S.isSymbolChar

-- | Test character is space.
isSpace :: O.Test Char
isSpace = Ch.isSpace

-- | Test character is double-quote.
isQQ :: O.Test Char
isQQ = ( == '"' )

-- | Test character is term slash.
isTerm :: O.Test Char
isTerm = ( == '/' )

-- | Test character is content line, i.e., @\'-\'@ or @\'=\'@.
isJudge :: O.Test Char
isJudge = ( `elem` "-=" )  -- Punctuation | Symbol

-- | Test character is component of clock text.
isClock :: O.Test Char
isClock c = Ch.isDigit c || c `elem` ".:'+-"


-- ============================================  Type

-- | Code scanner for token list.
type TokenScan = B.CodeScan String S.Token

-- | Read single token.
type TokenScanMap = O.Map TokenScan

-- | Clip single-token result.
type ClipResult = (S.InputText, S.Token)

-- | Clip single-token result with word cache.
type ClipResultC = (B.WordCache, S.InputText, S.Token)

-- | Clip multiple-tokens result with word cache.
type ClipResultCL = (B.WordCache, S.InputText, [S.Token])

-- | Clip a next token.
type ClipToken = B.CodePos -> S.InputText -> ClipResult

-- | Clip a next token with word cache.
type ClipTokenC = B.CodePos -> B.WordCache -> S.InputText -> ClipResultC

-- | Clip a next token with word cache.
type ClipTokenCL = B.CodePos -> B.WordCache -> S.InputText -> ClipResultCL


-- ============================================  Clip

-- | Update token scanner by clip result.
clipUpdate :: TokenScan -> ClipResult -> TokenScan
clipUpdate r (cs, tok) = B.codeUpdate cs tok r

-- | Update token scanner by clip result with word cache.
clipUpdateC :: TokenScan -> ClipResultC -> TokenScan
clipUpdateC r (wtab, cs, tok) = B.codeUpdateWords wtab cs tok r

-- | Update token scanner by clip result with word cache.
clipUpdateCL :: TokenScan -> ClipResultCL -> TokenScan
clipUpdateCL r (wtab, cs, toks) = B.codeUpdateListWords wtab cs toks r

-- ---------------------------------  Textual

-- | Clip space token.
--
--   >>> clipSpace B.def "  foo bar baz"
--   ("foo bar baz", TSpace /0.0.0/ 3)
--
clipSpace :: ClipToken
clipSpace cp cs =
    let (cs', n) = S.nextSpace cs
    in (cs', S.TSpace cp $ n + 1)

-- | Clip single-quoted text.
--
--   >>> clipQ B.def O.emptyWordCache "foo bar baz"
--   (Cache 8 ["foo"] [], " bar baz", TText /0.0.0/ TextQ "foo")
--
clipQ :: ClipTokenC
clipQ cp wtab cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> symbolToken S.TextQ w cp wtab cs'
      Left a         -> (wtab, [], S.TUnknown cp cs a)

-- | Clip double-quoted text.
--
--   >>> clipQq B.def O.emptyWordCache "foo\" bar baz"
--   (Cache 8 ["foo"] [], " bar baz", TText /0.0.0/ TextQQ "foo")
--
clipQq :: ClipTokenC
clipQq cp wtab cs =
    case S.nextQQ cs of
      Left a         -> (wtab, [], S.TUnknown cp cs a)
      Right (cs', w) -> case O.cacheGet wtab w of
                          (wtab', w') -> (wtab', cs', S.TText cp S.TextQQ w')

-- | Clip quoted text before delimiter.
--   This function must be called after two single quotations.
--
--   >>> clipQn B.def B.emptyWordCache "'foo''' bar baz"
--   (Cache 8 ["foo"] [], " bar baz", TText /0.0.0/ TextQQ "foo")
--
clipQn :: ClipTokenC
clipQn cp wtab (countQuote -> (n, cs)) =
    case S.nextBefore (replicate (n + 2) '\'') cs of
      Left a         -> (wtab, [], S.TUnknown cp cs a)
      Right (cs', w) -> case O.cacheGet wtab w of
                          (wtab', w') -> (wtab', cs', S.TText cp S.TextQQ w')

countQuote :: (O.Textual t) => t -> (Int, t)
countQuote = loop 0 where
    loop n (O.tCut -> Just ('\'', t)) = loop (n + 1) t
    loop n t = (n, t)

-- ---------------------------------  Identifier

-- | Clip symbolic token.
--
--   >>> clipSymbol B.def O.emptyWordCache "foo bar baz"
--   (Cache 8 ["foo"] [], " bar baz", TText /0.0.0/ TextRaw "foo")
--
clipSymbol :: ClipTokenC
clipSymbol cp wtab cs =
    let (cs', sym) = S.nextSymbol cs
    in case sym of
         S.SymbolShort pre w  -> (wtab, cs', S.TShort cp pre w)
         S.SymbolCommon    w  -> symbolToken S.TextRaw w cp wtab cs'
         S.SymbolGeneral   w  -> symbolToken S.TextRaw w cp wtab cs'
         S.SymbolPlain     w  -> symbolToken S.TextRaw w cp wtab cs'
         S.SymbolNumeric   w  -> symbolToken S.TextRaw w cp wtab cs'
         S.SymbolUnknown   w  -> (wtab, [], S.unknownToken cp cs $ Msg.forbiddenInput w)

-- | Create symbolic token.
symbolToken :: S.TextForm -> String -> ClipTokenC
symbolToken f w cp wtab cs =
    case O.cacheGet wtab w of
      (wtab', w') -> (wtab', cs, S.TText cp f w')

-- | Clip slot name, like @\@foo@.
--
--   >>> clipSlot 1 B.def "foo bar baz"
--   (" bar baz", TSlot /0.0.0/ 1 "foo")
--
clipSlot :: Int -> ClipToken
clipSlot n cp cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> (cs', S.TSlot cp n w)
      Left a         -> ([], S.TUnknown cp cs a)

-- ---------------------------------  Term

-- | Clip signed term name.
--
--   >>> clipTermSign "+/" B.def Map.empty "foo bar baz"
--   (fromList [("foo","foo")], " bar baz", [TTerm <I0-L0-C0> "+/foo"])
--
clipTermSign :: String -> ClipTokenCL
clipTermSign = clipTerm False

-- | Clip term name.
--
--   >>> clipTermName B.def Map.empty "foo bar baz"
--   (fromList [("foo","foo")], " bar baz", [TTerm <I0-L0-C0> "/foo"])
--
clipTermName :: ClipTokenCL
clipTermName = clipTerm False "/"

-- | Clip quoted term.
--
--   >>> clipTermQ B.def Map.empty "foo bar baz"
--   (fromList [], " bar baz", [TText <I0-L0-C0> TextTerm "foo"])
--
clipTermQ :: ClipTokenCL
clipTermQ = clipTerm True "/"

-- | Clip term name or a term path.
clipTerm :: Bool -> String -> ClipTokenCL
clipTerm q slash cp wtab cs0 = word [] cs0 where
    word ns ccs@(O.tCut -> Just (c, cs))
        | c == '='    = call (S.nextSymbolPlain cs)  (\w -> nterm ns w)
        | isSymbol c  = call (S.nextSymbolPlain ccs) (\w -> term (w : ns))
        | isQQ c      = call (S.nextQQ cs)           (\w -> term (w : ns))
    word _ _          = (wtab, [], [S.unknownToken cp cs0 Msg.expPlainSym])
    call e f          = case e of
                          Right (cs', w) -> f w cs'
                          Left a         -> (wtab, [], [S.TUnknown cp cs0 a])

    nterm ns w cs'    = let w' = show (O.getIx cp) ++ (O.tAdd '=' w)
                        in term (w' : ns) cs'

    term ns (O.tCut -> Just (c, cs))
        | isTerm c    = word ns cs
    term [n] cs
        | not q       = case O.cacheGet wtab $ slash ++ n of
                          (wtab', n') -> (wtab', cs, [S.TTerm cp n'])
    term ns cs
        | q           = case ns of
                          [n] -> (wtab, cs, [S.TText cp S.TextTerm n])
                          _   -> (wtab, cs, [S.unknownToken cp cs0 Msg.expPlainSym])
        | otherwise   = (wtab, cs, termPath (S.TTerm cp <$> ns))

    termPath [t]      = [t]
    termPath ts       = [S.TClose cp "-)"] ++ ts ++ [S.TOpen cp "(-"]

-- ---------------------------------  Symbol

-- | Clip token beginning with @"|"@.
--
--   >>> clipBar B.def "-- C"
--   (" C", TText /0.0.0/ TextBar "|--")
--
--   >>> clipBar B.def "| ..."
--   ("...", TText /0.0.0/ TextRaw "||")
--
--   >>> clipBar B.def "12:00| ..."
--   (" ...", TText /0.0.0/ TextBar "|12:00|")
--
clipBar :: B.CodePos -> String -> ClipResult
clipBar cp cs0 = bar (0 :: Int) cs0 where
    text n = O.tAdd '|' $ O.tTake n cs0
    barToken n = S.TText cp S.TextBar $ text n
    rawToken n = S.TText cp S.TextRaw $ text n

    bar n (O.tCut -> Just (c, cs))
        | c == '|'              = bar   (n + 1) cs
        | n == 0 && isJudge c   = judge (n + 1) cs
        | n == 0 && isSymbol c  = clock (n + 1) cs
    bar n cs = (O.trimBegin cs, rawToken n)   -- '||'

    -- judgement sign, like |--, |-x
    judge n (O.tCut -> Just (c, cs))
        | isJudge c || Ch.isAlpha c  = judge (n + 1) cs
        | isSymbol c                 = clock n (c:cs)
    judge n cs                       = (cs, barToken n)

    -- clock, like |03:30|
    clock n (O.tCut -> Just (c, cs))
        | c == '|'                   = (cs, barToken (n + 1))
        | isClock c                  = clock (n + 1) cs
    clock n cs                       = (cs, barToken n)

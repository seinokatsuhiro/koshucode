{-# OPTIONS_GHC -Wall #-}

-- | Clip token text.

module Koshucode.Baala.Syntax.Token.Clip
  ( -- * Type
    TokenScan, TokenScanMap,
    TokenNip, TokenNipW, TokenNipLW,
    TokenNipResult, TokenNipWResult, TokenNipLWResult,

    -- * Utility
    isSymbol,
    isSpace,
    isQQ,
    isTerm,
    isJudge,
    isClock,

    -- * Nipper
    -- ** Updater
    nipUpdate, nipUpdateW, nipUpdateLW,
    -- ** Textual
    nipSpace,
    nipQ, nipQQ,
    -- ** Identifier
    nipSymbol,
    --symbolToken,
    nipSlot,
    -- ** Term name
    nipTermSign,
    nipTermName,
    nipTermQ,
    -- ** Symbol
    nipBar,
  ) where

import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Symbol.Message  as Msg
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- --------------------------------------------  Type

-- | Code roll for token.
type TokenScan = B.CodeScan String S.Token

-- | Read single token.
type TokenScanMap = O.Map TokenScan

-- | Nip result.
type TokenNipResult = (S.InputText, S.Token)

-- | Nip result with word table.
type TokenNipWResult = (B.WordCache, S.InputText, S.Token)

-- | Nip result with word table.
type TokenNipLWResult = (B.WordCache, S.InputText, [S.Token])

-- | Nip off a next token.
type TokenNip = B.CodePos -> S.InputText -> TokenNipResult

-- | Nip off a next token with word table.
type TokenNipW = B.CodePos -> B.WordCache -> S.InputText -> TokenNipWResult

-- | Nip off a next token with word table.
type TokenNipLW = B.CodePos -> B.WordCache -> S.InputText -> TokenNipLWResult


-- --------------------------------------------  Utility

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

-- --------------------------------------------  Nipper

-- | Update token scanner by nipper result.
nipUpdate :: TokenScan -> TokenNipResult -> TokenScan
nipUpdate r (cs, tok) = B.codeUpdate cs tok r

-- | Update token scanner by nipper result with word table.
nipUpdateW :: TokenScan -> TokenNipWResult -> TokenScan
nipUpdateW r (wtab, cs, tok) = B.codeUpdateWords wtab cs tok r

-- | Update token scanner by nipper result with word table.
nipUpdateLW :: TokenScan -> TokenNipLWResult -> TokenScan
nipUpdateLW r (wtab, cs, toks) = B.codeUpdateListWords wtab cs toks r

-- ----------------------  Textual

-- | Nip off space token.
--
--   >>> nipSpace B.def "  foo bar baz"
--   ("foo bar baz", TSpace <I0-L0-C0> 3)
--
nipSpace :: TokenNip
nipSpace cp cs =
    let (cs', n) = S.nextSpace cs
    in (cs', S.TSpace cp $ n + 1)

-- | Nip off a single-quoted text.
--
--   >>> nipQ B.def O.emptyWordCache "foo bar baz"
--   (Cache 8 ["foo"] [], " bar baz", TText /0.0.0/ TextQ "foo")
--
nipQ :: TokenNipW
nipQ cp wtab cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> symbolToken S.TextQ w cp wtab cs'
      Left a         -> (wtab, [], S.TUnknown cp cs a)

-- | Nip off a double-quoted text.
--
--   >>> nipQQ B.def O.emptyWordCache "foo\" bar baz"
--   (Cache 8 ["foo"] [], " bar baz", TText /0.0.0/ TextQQ "foo")
--
nipQQ :: TokenNipW
nipQQ cp wtab cs =
    case S.nextQQ cs of
      Left a         -> (wtab, [], S.TUnknown cp cs a)
      Right (cs', w) -> case O.cacheGet wtab w of
                          (wtab', w') -> (wtab', cs', S.TText cp S.TextQQ w')

-- ----------------------  Identifier

-- | Nip off symbolic token.
--
--   >>> nipSymbol B.def O.emptyWordCache "foo bar baz"
--   (Cache 8 ["foo"] [], " bar baz", TText /0.0.0/ TextRaw "foo")
--
nipSymbol :: TokenNipW
nipSymbol cp wtab cs =
    let (cs', sym) = S.nextSymbol cs
    in case sym of
         S.SymbolShort pre w  -> (wtab, cs', S.TShort cp pre w)
         S.SymbolCommon    w  -> symbolToken S.TextRaw w cp wtab cs'
         S.SymbolGeneral   w  -> symbolToken S.TextRaw w cp wtab cs'
         S.SymbolPlain     w  -> symbolToken S.TextRaw w cp wtab cs'
         S.SymbolNumeric   w  -> symbolToken S.TextRaw w cp wtab cs'
         S.SymbolUnknown   w  -> (wtab, [], S.unknownToken cp cs $ Msg.forbiddenInput w)

-- | Create symbolic token.
symbolToken :: S.TextForm -> String -> TokenNipW
symbolToken f w cp wtab cs =
    case O.cacheGet wtab w of
      (wtab', w') -> (wtab', cs, S.TText cp f w')

-- | Nip off a slot name, like @\@foo@.
--
--   >>> nipSlot 1 B.def "foo bar baz"
--   (" bar baz", TSlot /0.0.0/ 1 "foo")
--
nipSlot :: Int -> TokenNip
nipSlot n cp cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> (cs', S.TSlot cp n w)
      Left a         -> ([], S.TUnknown cp cs a)

-- ----------------------  Term

-- | Nip off a signed term name.
--
--   >>> nipTermSign "+/" B.def Map.empty "foo bar baz"
--   (fromList [("foo","foo")], " bar baz", [TTerm <I0-L0-C0> "+/foo"])
--
nipTermSign :: String -> TokenNipLW
nipTermSign = nipTerm False

-- | Nip off a term name.
--
--   >>> nipTermName B.def Map.empty "foo bar baz"
--   (fromList [("foo","foo")], " bar baz", [TTerm <I0-L0-C0> "/foo"])
--
nipTermName :: TokenNipLW
nipTermName = nipTerm False "/"

-- | Nip off a quoted term.
--
--   >>> nipTermQ B.def Map.empty "foo bar baz"
--   (fromList [], " bar baz", [TText <I0-L0-C0> TextTerm "foo"])
--
nipTermQ :: TokenNipLW
nipTermQ = nipTerm True "/"

-- | Nip off a term name or a term path.
nipTerm :: Bool -> String -> TokenNipLW
nipTerm q slash cp wtab cs0 = word [] cs0 where
    word ns ccs@(c:cs)
        | c == '='     = call (S.nextSymbolPlain cs)  (\w -> nterm ns w)
        | isSymbol c   = call (S.nextSymbolPlain ccs) (\w -> term (w : ns))
        | isQQ c       = call (S.nextQQ cs)           (\w -> term (w : ns))
    word _ _           = (wtab, [], [S.unknownToken cp cs0 Msg.expOrdSym])
    call e f           = case e of
                           Right (cs', w) -> f w cs'
                           Left a         -> (wtab, [], [S.TUnknown cp cs0 a])

    nterm ns w cs'     = let w' = show (O.getIx cp) ++ ('=' : w)
                         in term (w' : ns) cs'

    term ns (c:cs) | isTerm c = word ns cs
    term [n] cs
        | not q        = case O.cacheGet wtab $ slash ++ n of
                           (wtab', n') -> (wtab', cs, [S.TTerm cp n'])
    term ns cs
        | q            = case ns of
                           [n] -> (wtab, cs, [S.TText cp S.TextTerm n])
                           _   -> (wtab, cs, [S.unknownToken cp cs0 Msg.expOrdSym])
        | otherwise    = (wtab, cs, termPath (S.TTerm cp <$> ns))

    termPath [t]       = [t]
    termPath ts        = [S.TClose cp "-)"] ++ ts ++ [S.TOpen cp "(-"]


-- ----------------------  Symbol

-- | Nip off token beginning with @"|"@.
nipBar :: B.CodePos -> String -> String -> TokenNipResult
nipBar cp = bar where
    bar (c:cs) w
        | c == '|'                   = bar cs (c:w)
        | w == "|" && isJudge c      = judge cs [c, '|']
        | w == "|" && isSymbol c     = clock cs [c, '|']
    bar cs w                         = let cs' = O.trimBegin cs
                                       in (cs', S.TText cp S.TextRaw w)

    -- read judgement sign, like |--, |-x
    judge (c:cs) w
        | isJudge c || Ch.isAlpha c  = judge cs (c:w)
        | isSymbol c                 = clock (c:cs) w
    judge cs w                       = (cs, token w)

    -- read clock, like |03:30|
    clock (c:cs) w | c == '|'        = (cs, token (c:w))
                   | isClock c       = clock cs (c:w)
    clock cs w                       = (cs, token w)

    token w = S.TText cp S.TextBar $ reverse w

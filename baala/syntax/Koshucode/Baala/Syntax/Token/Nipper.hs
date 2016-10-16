{-# OPTIONS_GHC -Wall #-}

-- | Token nipper.

module Koshucode.Baala.Syntax.Token.Nipper
  ( -- * Type
    TokenScan, TokenScanMap,
    TokenNip, TokenNipW,
    TokenNipResult, TokenNipWResult,

    -- * Utility
    isSymbol,
    isSpace,
    isQQ,
    isTerm,
    isJudge,
    isClock,

    -- * Nipper
    -- ** Updater
    nipUpdate, nipUpdateW,
    -- ** Textual
    nipSpace,
    nipQ, nipQQ,
    -- ** Identifier
    nipSymbol,
    symbolToken,
    nipSlot,
    -- ** Term name
    nipTermSign,
    nipTermPath,
    nipTermQ,
    -- ** Symbol
    nipBar,
  ) where

import qualified Data.Map                               as Map
import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Pattern   as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Symbol.Message  as Msg
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- --------------------------------------------  Type

-- | Code roll for token.
type TokenScan = B.CodeScan String S.Token

-- | Read single token.
type TokenScanMap = O.Map TokenScan

type TokenNipResult = (S.InputText, S.Token)

type TokenNipWResult = (B.WordTable, S.InputText, S.Token)

-- | Nip off a next token.
type TokenNip = B.CodePt -> S.InputText -> TokenNipResult

-- | Nip off a next token with word table.
type TokenNipW = B.CodePt -> B.WordTable -> S.InputText -> TokenNipWResult


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

-- ----------------------  Textual

-- | Nip off space token.
nipSpace :: TokenNip
nipSpace cp cs =
    let (cs', n) = S.nextSpace cs
    in (cs', S.TSpace cp $ n + 1)

-- | Nip off a single-quoted text.
nipQ :: TokenNipW
nipQ cp wtab cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> symbolToken S.TTextQ w cp wtab cs'
      Left a         -> (wtab, [], S.TUnknown cp cs a)
          

-- | Nip off a double-quoted text.
nipQQ :: TokenNip
nipQQ cp cs = case S.nextQQ cs of
                Right (cs', w) -> (cs', S.TTextQQ cp w)
                Left a         -> ([], S.TUnknown cp cs a)

-- ----------------------  Identifier

-- | Nip off symbolic token.
nipSymbol :: TokenNipW
nipSymbol cp wtab cs =
    let (cs', sym) = S.nextSymbol cs
    in case sym of
         S.SymbolShort pre w  -> (wtab, cs', S.TShort cp pre w)
         S.SymbolCommon    w  -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolGeneral   w  -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolPlain     w  -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolNumeric   w  -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolUnknown   w  -> (wtab, [], S.unknownToken cp cs $ Msg.forbiddenInput w)

-- | Create symbolic token.
symbolToken :: (B.CodePt -> String -> S.Token) -> String -> TokenNipW
symbolToken k w cp wtab cs =
    case Map.lookup w wtab of
         Just w' -> (wtab, cs, k cp w')
         Nothing -> let wtab' = Map.insert w w wtab
                    in (wtab', cs, k cp w)

-- | Nip off a slot name, like @\@foo@.
nipSlot :: Int -> TokenNip
nipSlot n cp cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> (cs', S.TSlot cp n w)
      Left a         -> ([], S.TUnknown cp cs a)

-- ----------------------  Term

-- | Nip off a signed term name.
nipTermSign :: Ordering -> TokenNipW
nipTermSign = nipTerm S.TermTypePath

-- | Nip off a term name.
nipTermPath :: TokenNipW
nipTermPath = nipTerm S.TermTypePath EQ

-- | Nip off a quoted term.
nipTermQ :: TokenNipW
nipTermQ = nipTerm S.TermTypeQuoted EQ

-- | Nip off a term name or a term path.
nipTerm :: S.TermType -> Ordering -> TokenNipW
nipTerm q sign cp wtab cs0 = word [] cs0 where
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
    term ns cs         = (wtab, cs, S.TTerm cp q $ reverse ns)

-- ----------------------  Symbol

-- | Nip off token beginning with @"|"@.
nipBar :: B.CodePt -> String -> String -> TokenNipResult
nipBar cp = bar where
    bar (c:cs) w
        | c == '|'                   = bar cs (c:w)
        | w == "|" && isJudge c      = judge cs [c, '|']
        | w == "|" && isSymbol c     = clock cs [c, '|']
    bar cs w                         = let cs' = O.trimLeft cs
                                       in (cs', S.TTextRaw cp w)

    -- read judgement sign, like |--, |-x
    judge (c:cs) w
        | isJudge c || Ch.isAlpha c  = judge cs (c:w)
        | isSymbol c                 = clock (c:cs) w
    judge cs w                       = (cs, S.TTextBar cp $ reverse w)

    -- read clock, like |03:30|
    clock (c:cs) w | c == '|'        = (cs, S.TTextBar cp $ reverse (c:w))
                   | isClock c       = clock cs (c:w)
    clock cs w                       = (cs, S.TTextBar cp $ reverse w)


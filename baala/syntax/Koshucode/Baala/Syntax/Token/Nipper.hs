{-# OPTIONS_GHC -Wall #-}

-- | Token nipper.

module Koshucode.Baala.Syntax.Token.Nipper
  ( -- * Type
    TokenRoll, TokenRollMap,
    TokenNip, TokenNipW,

    -- * Nipper
    scan, scanW,
    nipSpace,
    nipSymbol,
    symbolToken,
    nipQ, nipQQ,
    nipSlot,
    nipTermSign,
    nipTermPath,
    nipTermQ,
    isSymbol,
    isSpace,
  ) where

import qualified Data.Map                               as Map
import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Pattern   as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Symbol.Message  as Msg
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- --------------------------------------------  Type

-- | Code roll for token.
type TokenRoll = B.CodeScan String S.Token

-- | Read single token.
type TokenRollMap = B.Map TokenRoll

-- | Nip off a next token.
type TokenNip = B.CodePt -> S.InputText -> (S.InputText, S.Token)

-- | Nip off a next token with word table.
type TokenNipW = B.CodePt -> B.WordTable -> S.InputText -> (B.WordTable, S.InputText, S.Token)


-- --------------------------------------------  Nipper

-- | Update token scanner by nipper result.
scan :: TokenRoll -> (S.InputText, S.Token) -> TokenRoll
scan r (cs, tok) = B.codeUpdate cs tok r

-- | Update token scanner by nipper result with word table.
scanW :: TokenRoll -> (B.WordTable, S.InputText, S.Token) -> TokenRoll
scanW r (wtab, cs, tok) = B.codeUpdateWords wtab cs tok r

-- | Nip off space token.
nipSpace :: TokenNip
nipSpace cp cs =
    let (cs', n) = S.nextSpace cs
    in (cs', S.TSpace cp $ n + 1)

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

-- | Nip off a slot name, like @aaa.
nipSlot :: Int -> TokenNip
nipSlot n cp cs =
    case S.nextSymbolPlain cs of
      Right (cs', w) -> (cs', S.TSlot cp n w)
      Left a         -> ([], S.TUnknown cp cs a)

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

-- | Test character is symbolic.
isSymbol :: B.Pred Char
isSymbol = S.isSymbolChar

-- | Test character is space.
isSpace :: B.Pred Char
isSpace = Ch.isSpace

-- Punctuations
isQQ, isTerm :: B.Pred Char
isQQ       = ( ==  '"'  )  -- Punctuation
isTerm     = ( ==  '/'  )  -- Punctuation


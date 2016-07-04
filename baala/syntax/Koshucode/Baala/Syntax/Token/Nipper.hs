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
    isSymbol,
    isSpace,
  ) where

import qualified Data.Map                               as Map
import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Pattern   as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- --------------------------------------------  Type

-- | Code roll for token.
type TokenRoll = B.CodeRoll S.Token

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

-- | Split space token.
nipSpace :: TokenNip
nipSpace cp cs =
    let (cs', n) = S.nextSpace cs
    in (cs', S.TSpace cp $ n + 1)

-- | Split symbolic token.
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

-- | Test character is symbolic.
isSymbol :: B.Pred Char
isSymbol = S.isSymbolChar

-- | Test character is space.
isSpace :: B.Pred Char
isSpace = Ch.isSpace


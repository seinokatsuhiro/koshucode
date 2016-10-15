{-# OPTIONS_GHC -Wall #-}

-- | Next character sequence.

module Koshucode.Baala.Syntax.Symbol.Next
  ( -- * Next
    InputText,
    Next, AbNext,
    nextSpace,
    nextQQ,

    -- * Symbol
    -- ** Data type
    Symbol (..),
    -- ** Symbol test
    isGeneralSymbol, isPlainSymbol, isNumericSymbol, isShortSymbol,
    -- ** Char test
    isSymbolChar, isGeneralChar, isPlainChar, isNumericChar,
    -- ** Next symbol
    nextSymbol, nextSymbolPlain,
  ) where

import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol.Message  as Msg

-- | Input data type.
type InputText = String

-- | Split next character sequence from input text.
type Next a = InputText -> (InputText, a)

-- | Split next character sequence from input text.
type AbNext a = InputText -> B.Ab (InputText, a)

-- Punctuations
isQQ, isSpace :: O.Test Char
isQQ       = (== '"')
isSpace    = Ch.isSpace

-- | Get next spaces.
nextSpace :: Next Int
nextSpace = loop 0 where
    loop n (c:cs) | isSpace c   = loop (n + 1) cs
    loop n cs                   = (cs, n)

-- | Get next double-quoted text.
nextQQ :: AbNext String
nextQQ = loop "" where
    loop w (c:cs) | isQQ c        = Right (cs, reverse w)
                  | otherwise     = loop (c:w) cs
    loop _ _                      = Msg.quotNotEnd


-- --------------------------------------------  Symbol

-- ----------------------  Data type

-- | Symbol class.
--
-- Relation between character classes and symbol classes.
-- The char class @number'@ means Unicode number except for @0-9@.
-- @G@ for general symbol, @P@ for plain, @N@ for numeric.
--
-- >   Char class   Symbol class
-- >   ------------ -----------------
-- >   "0-9"        ( G ) ( P ) ( N )
-- >   "-"          ( G ) ( P ) ( N )
-- >   letter       ( G )   P
-- >   mark         ( G )   P
-- >   number'      ( G )   P
-- >   "_?"         ( G )   P
-- >   "+"          ( G )         N
-- >   "*=<>~"        G
-- >   ".#"                       N
--
-- Partial order of symbol classes.
--
-- >                     ( GPN ) Common
-- >                    /      |
-- >           Plain ( GP )  ( GN ) Numeric
-- >                /  |    /  |
-- >   Short ( P.P )   |   /   |
-- >            :      |  /    |
-- >         General ( G )     |
-- >            :      :     ( N ) Numeric
-- >            :      :       :
-- >            ................ Unknown

data Symbol
    = SymbolCommon    String           -- ^ General-plain-numeric symbol
    | SymbolGeneral   String           -- ^ General symbol
    | SymbolPlain     String           -- ^ Plain symbol
    | SymbolNumeric   String           -- ^ Numeric symbol
    | SymbolShort     String String    -- ^ Short symbol (Plain @"."@ Plain)
    | SymbolUnknown   String           -- ^ Unknown symbol
      deriving (Show, Eq, Ord)


-- ----------------------  Symbol test

-- | Test symbol is general, in other words,
--   'SymbolCommon', 'SymbolPlain' or 'SymbolGeneral'.
isGeneralSymbol :: O.Test Symbol
isGeneralSymbol (SymbolCommon _)   = True
isGeneralSymbol (SymbolPlain _)    = True
isGeneralSymbol (SymbolGeneral _)  = True
isGeneralSymbol _                  = False

-- | Test symbol is plain, in other words,
--   'SymbolCommon' or 'SymbolPlain'.
isPlainSymbol :: O.Test Symbol
isPlainSymbol (SymbolCommon _)     = True
isPlainSymbol (SymbolPlain _)      = True
isPlainSymbol _                    = False

-- | Test symbol is numeric, in other words,
--   'SymbolCommon' or 'SymbolNumeric'.
isNumericSymbol :: O.Test Symbol
isNumericSymbol (SymbolCommon _)   = True
isNumericSymbol (SymbolNumeric _)  = True
isNumericSymbol _                  = False

-- | Test symbol is 'SymbolShort'.
isShortSymbol :: O.Test Symbol
isShortSymbol (SymbolShort _ _)    = True
isShortSymbol _                    = False


-- ----------------------  Char test

-- | Test character is a symbol component.
isSymbolChar :: O.Test Char
isSymbolChar c = isGeneralChar c || isNumericChar c

-- | Test character is a general-symbol component.
isGeneralChar :: O.Test Char
isGeneralChar = isCharG'

-- | Test character is a plain-symbol component.
isPlainChar :: O.Test Char
isPlainChar = isCharP'

-- | Test character is a numeric-symbol component.
isNumericChar :: O.Test Char
isNumericChar = isCharN'

isCharGpn, isCharDigit, isCharHyphen :: O.Test Char
isCharGpn    c  = isCharDigit c || isCharHyphen c
isCharDigit  c  = c >= '0' && c <= '9'
isCharHyphen c  = c == '-'

isCharGp :: O.Test Char
isCharGp c =
    case B.majorGeneralCategory c of
      B.UnicodeLetter    -> True
      B.UnicodeMark      -> True
      B.UnicodeNumber    -> True      -- include isCharDigit
      _                  -> c == '_' || c == '?'

isCharGn, isCharG, isCharN :: O.Test Char
isCharGn c   = c == '+'
isCharG  c   = c `elem` "*=<>~"
isCharN  c   = c == '.' || c == '#'

isCharGp', isCharP', isCharGn', isCharG', isCharN' :: O.Test Char
isCharGp' c  = isCharGp  c || isCharHyphen c
isCharP'     = isCharGp'
isCharGn' c  = isCharGpn c || isCharGn c
isCharG'  c  = isCharGp' c || isCharGn c || isCharG c
isCharN'  c  = isCharGn' c || isCharN  c


-- ---------------------- Next symbol

-- | Get next symbol.
nextSymbol :: Next Symbol
nextSymbol = symbolGpn "" where

    done w cs k           = (cs, k $ reverse w)

    -- General and Plain and Numeric
    symbolGpn w (c:cs)
        | isCharGpn c     = symbolGpn (c:w) cs
        | isCharGp  c     = symbolGp  (c:w) cs
        | isCharGn  c     = symbolGn  (c:w) cs
        | isCharG   c     = symbolG   (c:w) cs
        | isCharN   c     = symbolN   (c:w) cs
        | isSymbolChar  c     = symbolUnk (c:w) cs
    symbolGpn w cs        = done w cs SymbolCommon

    -- General and Plain
    symbolGp w (c:cs)
        | c == '.'        = short (reverse w) "" cs
        | isCharGp' c     = symbolGp  (c:w) cs
        | isCharG   c     = symbolG   (c:w) cs
        | isSymbolChar  c     = symbolUnk (c:w) cs
    symbolGp w cs         = done w cs SymbolPlain

    -- Plain "." Plain
    short pre w (c:cs)
        | isCharGp' c     = short pre (c:w) cs
        | isSymbolChar  c     = symbolUnk (c:w) cs
    short pre w cs        = done w cs $ SymbolShort pre

    -- General and Numeric
    symbolGn w (c:cs)
        | isCharGn' c     = symbolGn  (c:w) cs
        | isCharG   c     = symbolG   (c:w) cs
        | isCharN   c     = symbolN   (c:w) cs
        | isSymbolChar  c     = symbolUnk (c:w) cs
    symbolGn w cs         = done w cs SymbolNumeric

    -- General
    symbolG w (c:cs)
        | isCharG' c      = symbolG   (c:w) cs
        | isSymbolChar c      = symbolUnk (c:w) cs
    symbolG w cs          = done w cs SymbolGeneral

    -- Numeric
    symbolN w (c:cs)
        | isCharN' c      = symbolN   (c:w) cs
        | isSymbolChar c      = symbolUnk (c:w) cs
    symbolN w cs          = done w cs SymbolNumeric

    -- Unknown symbol
    symbolUnk w (c:cs)
        | isSymbolChar c      = symbolUnk (c:w) cs
    symbolUnk w cs        = done w cs SymbolUnknown

nextSymbolPlain :: AbNext String
nextSymbolPlain cs =
    case nextSymbol cs of
      (cs', sym) -> do w <- getSymbolPlain sym
                       Right (cs', w)

getSymbolPlain :: Symbol -> B.Ab String
getSymbolPlain (SymbolCommon w)  = Right w
getSymbolPlain (SymbolPlain  w)  = Right w
getSymbolPlain _                 = Msg.expOrdSym


{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Symbol type.

module Koshucode.Baala.Syntax.Symbol.Symbol
  ( -- * Symbol
    -- ** Data type
    Symbol (..),
    -- ** Symbol test
    isGeneralSymbol, isPlainSymbol, isNumericSymbol, isShortSymbol,
    -- ** Char test
    isSymbolChar, isGeneralChar, isPlainChar, isNumericChar,
    -- ** Next symbol
    nextSymbol, nextSymbolPlain,
  ) where

import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol.Next     as S
import qualified Koshucode.Baala.Syntax.Symbol.Message  as Msg


-- ============================================  Symbol

-- ---------------------------------  Data type

-- | Symbol class.
--
-- Relation between character classes and symbol classes.
-- The char class @number'@ means Unicode number except for @0-9@.
-- @G@ for general symbol, @P@ for plain, @N@ for numeric.
--
-- >   Char class   Symbol class        Symbol type
-- >   ------------ ------------------- -----------------
-- >   "0-9"        ( G ) ( P ) ( N )   SymbolCommon
-- >   "-"          ( G ) ( P ) ( N )   SymbolCommon
-- >   letter       ( G )   P           SymbolPlain
-- >   mark         ( G )   P           SymbolPlain
-- >   number'      ( G )   P           SymbolPlain
-- >   "_!?"        ( G )   P           SymbolPlain
-- >   "+"          ( G )         N     SymbolNumeric
-- >   "*=<>~"        G                 SymbolGeneral
-- >   ".#"                       N     SymbolNumeric
--
-- Partial order between the symbol classes.
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

data Symbol t
    = SymbolCommon   t     -- ^ __1.__ General-plain-numeric symbol
                           --
                           --   >>> nextSymbol "1"
                           --   ("", SymbolCommon "1")

    | SymbolPlain    t     -- ^ __2.__ Plain (include general) symbol
                           --
                           --   >>> nextSymbol "a"
                           --   ("", SymbolPlain "a")

    | SymbolNumeric  t     -- ^ __3.__ Numeric (include general) symbol
                           --
                           --   >>> nextSymbol "+"
                           --   ("", SymbolNumeric "+")

    | SymbolGeneral  t     -- ^ __4.__ General (not plain nor numeric) symbol
                           --
                           --   >>> nextSymbol "="
                           --   ("", SymbolGeneral "=")

    | SymbolShort    t t   -- ^ __5.__ Short symbol (Plain @"."@ Plain)
                           --
                           --   >>> nextSymbol "a.xxx"
                           --   ("", SymbolShort "a" "xxx")

    | SymbolUnknown  t     -- ^ __6.__ Unknown symbol
                           --
                           --   >>> nextSymbol "a+"
                           --   ("", SymbolUnknown "a+")

      deriving (Show, Eq, Ord)


-- ---------------------------------  Symbol test

-- | Test symbol is general, in other words,
--   'SymbolCommon', 'SymbolPlain' or 'SymbolGeneral'.
isGeneralSymbol :: O.Test (Symbol t)
isGeneralSymbol (SymbolCommon _)   = True
isGeneralSymbol (SymbolPlain _)    = True
isGeneralSymbol (SymbolGeneral _)  = True
isGeneralSymbol _                  = False

-- | Test symbol is plain, in other words,
--   'SymbolCommon' or 'SymbolPlain'.
isPlainSymbol :: O.Test (Symbol t)
isPlainSymbol (SymbolCommon _)     = True
isPlainSymbol (SymbolPlain _)      = True
isPlainSymbol _                    = False

-- | Test symbol is numeric, in other words,
--   'SymbolCommon' or 'SymbolNumeric'.
isNumericSymbol :: O.Test (Symbol t)
isNumericSymbol (SymbolCommon _)   = True
isNumericSymbol (SymbolNumeric _)  = True
isNumericSymbol _                  = False

-- | Test symbol is 'SymbolShort'.
isShortSymbol :: O.Test (Symbol t)
isShortSymbol (SymbolShort _ _)    = True
isShortSymbol _                    = False


-- ---------------------------------  Char test

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
isCharGp c = case O.majorGeneralCategory c of
               O.UnicodeLetter  -> True
               O.UnicodeMark    -> True
               O.UnicodeNumber  -> True  -- include isCharDigit
               _                -> c == '_' || c == '!' || c == '?'

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


-- ---------------------------------  Next symbol

-- | Get next symbol.
--
--   >>> nextSymbol "foo bar"
--   (" bar", SymbolPlain "foo")
--
--   >>> nextSymbol "0.50"
--   ("", SymbolNumeric "0.50")
--
--   >>> nextSymbol "= /a"
--   (" /a", SymbolGeneral "=")
--
nextSymbol :: S.Next (Symbol String)
nextSymbol cs0 = symbolGpn O.zero cs0 where
    done n cs k           = (cs, k $ O.tTake n cs0)

    -- General and Plain and Numeric
    symbolGpn n (O.tCut -> O.Jp c cs)
        | isCharGpn c     = symbolGpn n' cs
        | isCharGp  c     = symbolGp  n' cs
        | isCharGn  c     = symbolGn  n' cs
        | isCharG   c     = symbolG   n' cs
        | isCharN   c     = symbolN   n' cs
        | isSymbolChar c  = symbolUnk n' cs
        where n' = n + 1
    symbolGpn n cs        = done n cs SymbolCommon

    -- General and Plain
    symbolGp n (O.tCut -> O.Jp c cs)
        | c == '.'        = shortBody (O.tTake n cs0) cs
        | isCharGp' c     = symbolGp  n' cs
        | isCharG   c     = symbolG   n' cs
        | isSymbolChar c  = symbolUnk n' cs
        where n' = n + 1
    symbolGp n cs         = done n cs SymbolPlain

    -- General and Numeric
    symbolGn n (O.tCut -> O.Jp c cs)
        | isCharGn' c     = symbolGn  n' cs
        | isCharG   c     = symbolG   n' cs
        | isCharN   c     = symbolN   n' cs
        | isSymbolChar c  = symbolUnk n' cs
        where n' = n + 1
    symbolGn n cs         = done n cs SymbolNumeric

    -- General
    symbolG n (O.tCut -> O.Jp c cs)
        | isCharG' c      = symbolG   n' cs
        | isSymbolChar c  = symbolUnk n' cs
        where n' = n + 1
    symbolG n cs          = done n cs SymbolGeneral

    -- Numeric
    symbolN n (O.tCut -> O.Jp c cs)
        | isCharN' c      = symbolN   n' cs
        | isSymbolChar c  = symbolUnk n' cs
        where n' = n + 1
    symbolN n cs          = done n cs SymbolNumeric

    -- Unknown symbol
    symbolUnk n (O.tCut -> O.Jp c cs)
        | isSymbolChar c  = symbolUnk (n + 1) cs
    symbolUnk n cs        = done n cs SymbolUnknown

shortBody :: String -> S.Next (Symbol String)
shortBody pre cs0 = short O.zero cs0 where
    done n cs k           = (cs, k $ O.tTake n cs0)

    -- Plain "." Plain
    short n (O.tCut -> O.Jp c cs)
        | isCharGp' c     = short n' cs
        | isSymbolChar c  = symbolUnk n' cs
        where n' = n + 1
    short n cs            = done n cs $ SymbolShort pre

    -- Unknown symbol
    symbolUnk n (O.tCut -> O.Jp c cs)
        | isSymbolChar c  = symbolUnk (n + 1) cs
    symbolUnk n cs        = done n cs SymbolUnknown

-- | Get next plain symbol, i.e.,
--   'SymbolCommon' or 'SymbolPlain'.
--
--   >>> nextSymbolPlain "foo bar"
--   Right (" bar","foo")
--
--   >>> nextSymbolPlain "12.0"
--   Left (AbortReason "Expect ordinary symbol" ...)
--
nextSymbolPlain :: S.AbNext String
nextSymbolPlain cs = getSymbolPlain O.<#> nextSymbol cs

-- | Extract plain symbol text.
getSymbolPlain :: Symbol t -> B.Ab t
getSymbolPlain (SymbolCommon w)  = Right w
getSymbolPlain (SymbolPlain  w)  = Right w
getSymbolPlain _                 = Msg.expPlainSym


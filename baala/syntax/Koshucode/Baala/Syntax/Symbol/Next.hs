{-# LANGUAGE ViewPatterns #-}
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

-- | Test double punctuation character.
isQQ :: O.Test Char
isQQ = (== '"')

-- | Get next spaces.
--   Space character is decided by 'Ch.isSpace'.
--
--   >>> nextSpace " abc"
--   ("abc", 1)
--
--   >>> nextSpace " \tabc"
--   ("abc", 2)
--
nextSpace :: Next Int
nextSpace = loop 0 where
    loop n (O.uncons -> Just (c, cs))
        | Ch.isSpace c   = loop (n + 1) cs
    loop n cs            = (cs, n)

-- | Get next double quoted text.
--   A single double quote ends text.
--   A double double quote does not end text,
--   but it is converted to double quote.
--
--   >>> nextQQ "abc\" def"
--   Right (" def","abc")
--
--   >>> nextQQ "abc\"\"def\" ghi"
--   Right (" ghi","abc\"def")
--
nextQQ :: AbNext String
nextQQ cs0 = loop O.zero cs0 where
    loop n (O.uncons -> Just (c, cs))
        | isQQ c      = qq n cs
        | otherwise   = loop (n + 1) cs
    loop _ _          = Msg.quotNotEnd

    qq n (O.uncons -> Just (c, cs))
        | isQQ c      = do (cs', s') <- nextQQ cs
                           Right (cs', take (n + 1) cs0 ++ s')
    qq n cs           = Right (cs, take n cs0)


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
    = SymbolCommon    String         -- ^ __1.__ General-plain-numeric symbol
    | SymbolGeneral   String         -- ^ __2.__ General symbol
    | SymbolPlain     String         -- ^ __3.__ Plain symbol
    | SymbolNumeric   String         -- ^ __4.__ Numeric symbol
    | SymbolShort     String String  -- ^ __5.__ Short symbol (Plain @"."@ Plain)
    | SymbolUnknown   String         -- ^ __6.__ Unknown symbol
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
    case O.majorGeneralCategory c of
      O.UnicodeLetter    -> True
      O.UnicodeMark      -> True
      O.UnicodeNumber    -> True      -- include isCharDigit
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
nextSymbol :: Next Symbol
nextSymbol cs0 = symbolGpn (0 :: Int) cs0 where
    done n cs k           = (cs, k $ take n cs0)

    -- General and Plain and Numeric
    symbolGpn n (c:cs)
        | isCharGpn c     = symbolGpn n' cs
        | isCharGp  c     = symbolGp  n' cs
        | isCharGn  c     = symbolGn  n' cs
        | isCharG   c     = symbolG   n' cs
        | isCharN   c     = symbolN   n' cs
        | isSymbolChar c  = symbolUnk n' cs
        where n' = n + 1
    symbolGpn n cs        = done n cs SymbolCommon

    -- General and Plain
    symbolGp n (c:cs)
        | c == '.'        = shortBody (take n cs0) cs
        | isCharGp' c     = symbolGp  n' cs
        | isCharG   c     = symbolG   n' cs
        | isSymbolChar c  = symbolUnk n' cs
        where n' = n + 1
    symbolGp n cs         = done n cs SymbolPlain

    -- General and Numeric
    symbolGn n (c:cs)
        | isCharGn' c     = symbolGn  n' cs
        | isCharG   c     = symbolG   n' cs
        | isCharN   c     = symbolN   n' cs
        | isSymbolChar c  = symbolUnk n' cs
        where n' = n + 1
    symbolGn n cs         = done n cs SymbolNumeric

    -- General
    symbolG n (c:cs)
        | isCharG' c      = symbolG   n' cs
        | isSymbolChar c  = symbolUnk n' cs
        where n' = n + 1
    symbolG n cs          = done n cs SymbolGeneral

    -- Numeric
    symbolN n (c:cs)
        | isCharN' c      = symbolN   n' cs
        | isSymbolChar c  = symbolUnk n' cs
        where n' = n + 1
    symbolN n cs          = done n cs SymbolNumeric

    -- Unknown symbol
    symbolUnk n (c:cs)
        | isSymbolChar c  = symbolUnk (n + 1) cs
    symbolUnk n cs        = done n cs SymbolUnknown

shortBody :: String -> Next Symbol
shortBody pre cs0 = short (0 :: Int) cs0 where
    done n cs k           = (cs, k $ take n cs0)

    -- Plain "." Plain
    short n (c:cs)
        | isCharGp' c     = short n' cs
        | isSymbolChar c  = symbolUnk n' cs
        where n' = n + 1
    short n cs            = done n cs $ SymbolShort pre

    -- Unknown symbol
    symbolUnk n (c:cs)
        | isSymbolChar c  = symbolUnk (n + 1) cs
    symbolUnk n cs        = done n cs SymbolUnknown

-- | Get next plain symbol.
nextSymbolPlain :: AbNext String
nextSymbolPlain cs =
    case nextSymbol cs of
      (cs', sym) -> do w <- getSymbolPlain sym
                       Right (cs', w)

getSymbolPlain :: Symbol -> B.Ab String
getSymbolPlain (SymbolCommon w)  = Right w
getSymbolPlain (SymbolPlain  w)  = Right w
getSymbolPlain _                 = Msg.expOrdSym


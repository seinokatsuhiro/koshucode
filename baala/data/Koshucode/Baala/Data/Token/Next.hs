{-# OPTIONS_GHC -Wall #-}

-- | Next character sequence.

module Koshucode.Baala.Data.Token.Next
  ( -- * Data type
    InputText,
    Next, AbNext,
    nextSpace,
    nextQQ,

    -- * Symbol
    Symbol (..),
    nextSymbol, nextSymbolPlain,
    isSymbol, isGeneral, isPlain, isNumeric,
  ) where

import qualified Data.Char                            as Ch
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Data.Token.Message   as Msg

-- | Input data type.
type InputText = String

-- | Split next character sequence from input text.
type Next a = InputText -> (InputText, a)

-- | Split next character sequence from input text.
type AbNext a = InputText -> B.Ab (InputText, a)

-- Punctuations
isQQ, isSpace :: B.Pred Char
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
    = SymbolCommon    String           -- ^ General-ordinary-numeric symbol
    | SymbolGeneral   String           -- ^ General symbol
    | SymbolPlain     String           -- ^ Plain symbol
    | SymbolNumeric   String           -- ^ Numeric symbol
    | SymbolShort     String String    -- ^ Short symbol (Plain @"."@ Plain)
    | SymbolUnknown   String           -- ^ Unknown symbol
      deriving (Show, Eq, Ord)

isCharGpn, isCharDigit, isCharHyphen :: Char -> Bool
isCharGpn    c  = isCharDigit c || isCharHyphen c
isCharDigit  c  = c >= '0' && c <= '9'
isCharHyphen c  = c == '-'

isCharGp :: Char -> Bool
isCharGp c =
    case B.majorGeneralCategory c of
      B.UnicodeLetter    -> True
      B.UnicodeMark      -> True
      B.UnicodeNumber    -> True      -- include isCharDigit
      _                  -> c == '_' || c == '?'

isCharGn, isCharG, isCharN :: Char -> Bool
isCharGn c   = c == '+'
isCharG  c   = c `elem` "*=<>~"
isCharN  c   = c == '.' || c == '#'

isCharGp', isCharP', isCharGn', isCharG', isCharN' :: Char -> Bool
isCharGp' c  = isCharGp  c || isCharHyphen c
isCharP'     = isCharGp'
isCharGn' c  = isCharGpn c || isCharGn c
isCharG'  c  = isCharGp' c || isCharGn c || isCharG c
isCharN'  c  = isCharGn' c || isCharN  c

-- | Test character is a symbol component.
isSymbol :: Char -> Bool
isSymbol c = isGeneral c || isNumeric c

-- | Test character is a general-symbol component.
isGeneral :: Char -> Bool
isGeneral = isCharG'

-- | Test character is a plain-symbol component.
isPlain :: Char -> Bool
isPlain = isCharP'

-- | Test character is a numeric-symbol component.
isNumeric :: Char -> Bool
isNumeric = isCharN'

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
        | isSymbol  c     = symbolUnk (c:w) cs
    symbolGpn w cs        = done w cs SymbolCommon

    -- General and Plain
    symbolGp w (c:cs)
        | c == '.'        = short (reverse w) "" cs
        | isCharGp' c     = symbolGp  (c:w) cs
        | isCharG   c     = symbolG   (c:w) cs
        | isSymbol  c     = symbolUnk (c:w) cs
    symbolGp w cs         = done w cs SymbolPlain

    -- Plain "." Plain
    short pre w (c:cs)
        | isCharGp' c     = short pre (c:w) cs
        | isSymbol  c     = symbolUnk (c:w) cs
    short pre w cs        = done w cs $ SymbolShort pre

    -- General and Numeric
    symbolGn w (c:cs)
        | isCharGn' c     = symbolGn  (c:w) cs
        | isCharG   c     = symbolG   (c:w) cs
        | isCharN   c     = symbolN   (c:w) cs
        | isSymbol  c     = symbolUnk (c:w) cs
    symbolGn w cs         = done w cs SymbolNumeric

    -- General
    symbolG w (c:cs)
        | isCharG' c      = symbolG   (c:w) cs
        | isSymbol c      = symbolUnk (c:w) cs
    symbolG w cs          = done w cs SymbolGeneral

    -- Numeric
    symbolN w (c:cs)
        | isCharN' c      = symbolN   (c:w) cs
        | isSymbol c      = symbolUnk (c:w) cs
    symbolN w cs          = done w cs SymbolNumeric

    -- Unknown symbol
    symbolUnk w (c:cs)
        | isSymbol c      = symbolUnk (c:w) cs
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


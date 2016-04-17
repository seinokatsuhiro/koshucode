{-# OPTIONS_GHC -Wall #-}

-- | Next character sequence.

module Koshucode.Baala.Data.Token.Next
  ( -- * Data type
    InputText,
    Next, AbNext,
    nextSpace,
    nextQQ,
    nextGeneral,

    -- * Symbol
    Symbol (..),
    nextSymbol,
    nextSymbolOrdinary,
    isSymbol,
  ) where

import qualified Data.Char                            as Ch
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Data.Token.Short     as D
import qualified Koshucode.Baala.Data.Token.Message   as Msg

-- | Input data type.
type InputText = String

-- | Split next character sequence from input text.
type Next a = InputText -> (InputText, a)

-- | Split next character sequence from input text.
type AbNext a = InputText -> B.Ab (InputText, a)

-- Punctuations
isQQ, isSpace, isGeneral :: B.Pred Char
isQQ       = (== '"')
isSpace    = Ch.isSpace
isGeneral  = D.isGeneralChar

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

-- | Get next general sign.
nextGeneral :: Next String
nextGeneral = loop "" where
    loop w (c:cs) | isGeneral c   = loop (c:w) cs
    loop w cs                     = (cs, reverse w)


-- --------------------------------------------  Symbol

data Symbol
    = SymbolCommon    String           -- ^ General-ordinary-numeric symbol
    | SymbolGeneral   String           -- ^ General symbol
    | SymbolOrdinary  String           -- ^ Ordinary symbol
    | SymbolNumeric   String           -- ^ Numeric symbol
    | SymbolShort     String String    -- ^ Short symbol (Ordinary "." Ordinary)
    | SymbolUnknown   String           -- ^ Unknown symbol
      deriving (Show, Eq, Ord)

--  Classification of character classes
--
--    Char class   Symbol class
--    ------------ ------------
--    "0-9"        (G) (O) (N)
--    "-"          (G) (O) (N)
--    letter       (G)  O 
--    mark         (G)  O 
--    number'      (G)  O 
--    "_"          (G)  O 
--    "+"          (G)      N
--    "*=<>"        G   
--    ".:#"                 N

isSymbolGon :: Char -> Bool
isSymbolGon c = (c >= '0' && c <= '9') || c == '-'

isSymbolGo :: Char -> Bool
isSymbolGo c =
    case B.majorGeneralCategory c of
      B.UnicodeLetter    -> True
      B.UnicodeMark      -> True
      B.UnicodeNumber    -> True
      _                  -> c `elem` "_"

isSymbolGo' :: Char -> Bool
isSymbolGo' c = isSymbolGo c || isSymbolGon c

isSymbolO' :: Char -> Bool
isSymbolO' = isSymbolGo'

isSymbolGn :: Char -> Bool
isSymbolGn c = c == '+'

isSymbolGn' :: Char -> Bool
isSymbolGn' c = isSymbolGon c || isSymbolGn c

isSymbolG :: Char -> Bool
isSymbolG c = c `elem` "*=<>"

isSymbolG' :: Char -> Bool
isSymbolG' c = isSymbolGo c || isSymbolGn c || isSymbolG c

isSymbolN :: Char -> Bool
isSymbolN c = c `elem` ".#"

isSymbolN' :: Char -> Bool
isSymbolN' c = isSymbolGon c || isSymbolGn c || isSymbolN c

-- | Test character is symbol component.
isSymbol :: Char -> Bool
isSymbol c = isSymbolG' c || isSymbolO' c || isSymbolN' c

--  Partial order of symbol classes
--
--      GON
--     /   |
--    GO   GN
--     | / |
--     G   |
--     |   N
--     |   |
--     empty

-- | Get next symbol.
nextSymbol :: Next Symbol
nextSymbol = symbolGon "" where

    done w cs k           = (cs, k $ reverse w)

    -- General and Ordinary and Numeric
    symbolGon w (c:cs)
        | isSymbolGon c   = symbolGon (c:w) cs
        | isSymbolGo  c   = symbolGo  (c:w) cs
        | isSymbolGn  c   = symbolGn  (c:w) cs
        | isSymbolG   c   = symbolG   (c:w) cs
        | isSymbolN   c   = symbolN   (c:w) cs
        | isSymbol    c   = symbolUnk (c:w) cs
    symbolGon w cs        = done w cs SymbolCommon

    -- General and Ordinary
    symbolGo w (c:cs)
        | c == '.'        = short (reverse w) "" cs
        | isSymbolGo' c   = symbolGo  (c:w) cs
        | isSymbolG   c   = symbolG   (c:w) cs
        | isSymbol    c   = symbolUnk (c:w) cs
    symbolGo w cs         = done w cs SymbolOrdinary

    -- Ordinary "." Ordinary
    short pre w (c:cs)
        | isSymbolGo' c   = short pre (c:w) cs
        | isSymbol    c   = symbolUnk (c:w) cs
    short pre w cs        = done w cs $ SymbolShort pre

    -- General and Numeric
    symbolGn w (c:cs)
        | isSymbolGn' c   = symbolGn  (c:w) cs
        | isSymbolG   c   = symbolG   (c:w) cs
        | isSymbolN   c   = symbolN   (c:w) cs
        | isSymbol    c   = symbolUnk (c:w) cs
    symbolGn w cs         = done w cs SymbolNumeric

    -- General
    symbolG w (c:cs)
        | isSymbolG' c    = symbolG   (c:w) cs
        | isSymbol   c    = symbolUnk (c:w) cs
    symbolG w cs          = done w cs SymbolGeneral

    -- Numeric
    symbolN w (c:cs)
        | isSymbolN' c    = symbolN   (c:w) cs
        | isSymbol   c    = symbolUnk (c:w) cs
    symbolN w cs          = done w cs SymbolNumeric

    -- Unknown symbol
    symbolUnk w (c:cs)
        | isSymbol c      = symbolUnk (c:w) cs
    symbolUnk w cs        = done w cs SymbolUnknown

nextSymbolOrdinary :: AbNext String
nextSymbolOrdinary cs =
    case nextSymbol cs of
      (cs', sym) -> do w <- getSymbolOrdinary sym
                       Right (cs', w)

getSymbolOrdinary :: Symbol -> B.Ab String
getSymbolOrdinary (SymbolCommon   w)  = Right w
getSymbolOrdinary (SymbolOrdinary w)  = Right w
getSymbolOrdinary _                   = Msg.expOrdSym


{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokens in Koshucode.

module Koshucode.Baala.Base.Token.Token
(
  -- * Token type
  Token (..),
  textToken,

  -- * Term name
  TermName, TermName2, TermName3, TermName4,
  Terminal, TermPath,

  -- * Selectors
  tokenContent,
  tokenTypeText,

  -- * Predicates
  isBlankToken, isShortToken, isTermToken,
  isOpenTokenOf, isCloseTokenOf,

  -- * Other function
  sweepToken,
) where

import qualified Data.Generics                  as G
import qualified Koshucode.Baala.Base.Prelude   as B
import qualified Koshucode.Baala.Base.Text      as B


-- ----------------------  Token type

-- | There are nine types of tokens.
data Token
    = TText    B.CodePoint Int String     -- ^ Text.
                                          --   'Int' represents quotation level, i.e.,
                                          --   0 for non-quoted,
                                          --   1 for single-quoted,
                                          --   2 for double-quoted,
                                          --   3 for @-with@ variable.
    | TName    B.CodePoint String         -- ^ Operator name.
    | TSlot    B.CodePoint Int String     -- ^ Slot name.
                                          --   'Int' represents slot level, i.e.,
                                          --   0 for positional slots,
                                          --   1 for named slots,
                                          --   2 for global slots.
    | TShort   B.CodePoint String String  -- ^ Abbreviated text.
    | TTerm    B.CodePoint TermPath       -- ^ Term name.
    | TOpen    B.CodePoint String         -- ^ Opening paren.
    | TClose   B.CodePoint String         -- ^ Closing paren.
    | TSpace   B.CodePoint Int            -- ^ /N/ space characters.
    | TComment B.CodePoint String         -- ^ Comment text.
    | TUnknown B.CodePoint String         -- ^ Unknown text.
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Name Token where
    name (TTerm   _ ns) = concat ns
    name (TSlot  _ _ s) = s
    name (TText  _ _ s) = s
    name (TOpen    _ s) = s
    name (TClose   _ s) = s
    name (TComment _ s) = s
    name x = error $ "unknown name: " ++ show x

instance B.Write Token where
    write sh = d where
        d (TText    pt q w) = pretty "TText"    pt [show q, show w]
        d (TName    pt w)   = pretty "TName"    pt [show w]
        d (TShort   pt a b) = pretty "TShort"   pt [show a, show b]
        d (TTerm    pt ns)  = pretty "TTerm"    pt [show ns]
        d (TSlot    pt n w) = pretty "TSlot"    pt [show n, show w]
        d (TOpen    pt p)   = pretty "TOpen"    pt [show p]
        d (TClose   pt p)   = pretty "TClose"   pt [show p]
        d (TSpace   pt c)   = pretty "TSpace"   pt [show c]
        d (TComment pt s)   = pretty "TComment" pt [show s]
        d (TUnknown pt s)   = pretty "TUnknown" pt [show s]
        pretty k pt xs      = B.writeH sh $ k : lineCol pt : xs
        lineCol pt          = (show $ B.codePointLineNumber pt)
                              ++ ":" ++ (show $ B.codePointColumnNumber pt)

textToken :: String -> Token
textToken = TText B.codePointZero 0

instance B.CodePointer Token where
    codePoints (TText    cp _ _)  =  [cp]
    codePoints (TName    cp _)    =  [cp]
    codePoints (TShort   cp _ _)  =  [cp]
    codePoints (TTerm    cp _)    =  [cp]
    codePoints (TSlot    cp _ _)  =  [cp]
    codePoints (TOpen    cp _)    =  [cp]
    codePoints (TClose   cp _)    =  [cp]
    codePoints (TSpace   cp _)    =  [cp]
    codePoints (TComment cp _)    =  [cp]
    codePoints (TUnknown cp _)    =  [cp]



-- ---------------------- Term name

-- | Name of term, e.g., @\"file\"@ for the term name @\/file@.
type TermName    =  String
type TermName2   =  (String, String)
type TermName3   =  (String, String, String)
type TermName4   =  (String, String, String, String)

-- | Pair of term name and something.
type Terminal a  =  (TermName, a)

-- | Path of term names, e.g., term name @\/r\/x@
--   is correspond to path @[\"r\", \"x\"]@.
type TermPath    =  [TermName]


-- ---------------------- Selector

-- | Get the content of token.
--
--   >>> let tok = TTerm B.codePointZero ["r", "x"] in tokenContent tok
--   "/r/x"
tokenContent :: Token -> String
tokenContent (TText  _ _ s)   =  s
tokenContent (TName    _ s)   =  s
tokenContent (TShort _ a b)   =  a ++ "." ++ b
tokenContent (TTerm    _ ns)  =  concatMap ('/' :) ns
tokenContent (TSlot  _ _ s)   =  s
tokenContent (TOpen    _ s)   =  s
tokenContent (TClose   _ s)   =  s
tokenContent (TSpace   _ n)   =  replicate n ' '
tokenContent (TComment _ s)   =  s
tokenContent (TUnknown _ s)   =  s

-- | Text of token type, i.e., one of
--   @\"Text\"@, @\"Term\"@, @\"Open\"@, @\"Close\"@,
--   @\"Space\"@, @\"Comment\"@, or @\"Unknown\"@.
--
--   >>> tokenTypeText $ textToken "flower"
--   "Text"
tokenTypeText :: Token -> String
tokenTypeText (TText  _ _ _)  =  "text"
tokenTypeText (TName    _ _)  =  "name"
tokenTypeText (TShort _ _ _)  =  "short"
tokenTypeText (TTerm    _ _)  =  "term"
tokenTypeText (TSlot  _ _ _)  =  "slot"
tokenTypeText (TOpen    _ _)  =  "open"
tokenTypeText (TClose   _ _)  =  "close"
tokenTypeText (TSpace   _ _)  =  "space"
tokenTypeText (TComment _ _)  =  "comment"
tokenTypeText (TUnknown _ _)  =  "unknown"


-- ----------------------  Predicate

-- | Test the token is blank,
--   i.e., 'TComment' or 'TSpace'.
isBlankToken :: B.Pred Token
isBlankToken (TSpace _ _)    = True
isBlankToken (TComment _ _)  = True
isBlankToken _               = False

-- | Remove blank tokens.
sweepToken :: B.Map [Token]
sweepToken = B.omit isBlankToken

isShortToken :: B.Pred Token
isShortToken (TShort _ _ _)  = True
isShortToken _               = False

isTermToken :: B.Pred Token
isTermToken (TTerm _ _) = True
isTermToken _           = False

-- | Check token is a 'TOpen' of the specific paren.
--
--   >>> let tok = TOpen 0 "(" in isOpenTokenOf "(" tok
--   True
--
--   >>> let tok = TOpen 0 "{" in isOpenTokenOf "(" tok
--   False
isOpenTokenOf :: String -> B.Pred Token
isOpenTokenOf p1 (TOpen _ p2)   = p1 == p2
isOpenTokenOf _ _               = False

-- | Check token is a 'TClose' of the specific paren.
isCloseTokenOf :: String -> B.Pred Token
isCloseTokenOf p1 (TClose _ p2) = p1 == p2
isCloseTokenOf _ _              = False


{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokens in Koshucode.

module Koshucode.Baala.Base.Token.Token
(
  -- * Token type
  Token (..),
  TokenListing (..),
  TermName,
  TermName2,
  TermName3,
  TermName4,
  tokenWord,

  -- * Selectors
  tokenPos,
  tokenContent,
  tokenTypeText,

  -- * Predicates
  isBlankToken,
  isShortToken,
  isOpenTokenOf,
  isCloseTokenOf,

  -- * Other function
  sweepToken,
  tokenIndent,
) where

import qualified Data.Generics                       as G
import qualified Koshucode.Baala.Base.Prelude        as B
import qualified Koshucode.Baala.Base.Text           as B
import qualified Koshucode.Baala.Base.Token.TokenPos as B



-- ----------------------  Token type

-- | There are eight types of tokens.
data Token
    = TWord    B.TokenPos Int String
               -- ^ Word.
               --   'Int' represents quotation level, i.e.,
               --   0 for non-quoted,
               --   1 for single-quoted,
               --   2 for double-quoted.
    | TShort   B.TokenPos String String  -- ^ Abbreviated word
    | TTerm    B.TokenPos [TermName]  -- ^ Term name
    | TOpen    B.TokenPos String      -- ^ Opening paren
    | TClose   B.TokenPos String      -- ^ Closing paren
    | TSpace   B.TokenPos Int         -- ^ /N/ space characters
    | TComment B.TokenPos String      -- ^ Comment text
    | TUnknown B.TokenPos String      -- ^ Unknown text
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Name of term, e.g., @\"file\"@ for the term @\/file@.
type TermName  = String
type TermName2 = (String, String)
type TermName3 = (String, String, String)
type TermName4 = (String, String, String, String)

instance B.Name Token where
    name (TTerm   _ ns) = concat ns
    name (TWord  _ _ s) = s
    name (TOpen    _ s) = s
    name (TClose   _ s) = s
    name (TComment _ s) = s
    name x = error $ "unknown name: " ++ show x

instance B.Pretty Token where
    doc = d where
        d (TWord    pos q w) = pretty "TWord"    pos [show q, show w]
        d (TShort   pos a b) = pretty "TShort"   pos [show a, show b]
        d (TTerm    pos ns)  = pretty "TTerm"    pos [show ns]
        d (TOpen    pos p)   = pretty "TOpen"    pos [show p]
        d (TClose   pos p)   = pretty "TClose"   pos [show p]
        d (TSpace   pos c)   = pretty "TSpace"   pos [show c]
        d (TComment pos s)   = pretty "TComment" pos [show s]
        d (TUnknown pos s)   = pretty "TUnknown" pos [show s]
        pretty k pos xs = B.doch $ k : lineCol pos : xs
        lineCol pos = (show $ B.tokenPosLineNumber pos)
                      ++ ":" ++ (show $ B.tokenPosColumn pos)

tokenWord :: String -> Token
tokenWord = TWord B.tokenPosZero 0

class TokenListing a where
    tokenListing :: a -> [Token]

instance (TokenListing a) => TokenListing (Maybe a) where
    tokenListing (Nothing) = []
    tokenListing (Just a)  = tokenListing a

-- ---------------------- Selector

tokenPos :: Token -> B.TokenPos
tokenPos (TWord    p _ _)  = p
tokenPos (TShort   p _ _)  = p
tokenPos (TTerm    p _)    = p
tokenPos (TOpen    p _)    = p
tokenPos (TClose   p _)    = p
tokenPos (TSpace   p _)    = p
tokenPos (TComment p _)    = p
tokenPos (TUnknown p _)    = p

-- | Get the content of token.
--
--   >>> let tok = TTerm 20 ["/r", "/x"] in tokenContent tok
--   "/r/x"
tokenContent :: Token -> String
tokenContent (TWord  _ _ s)   = s
tokenContent (TShort _ a b)   = a ++ "." ++ b
tokenContent (TTerm    _ ns)  = concat $ map ('/':) ns
tokenContent (TOpen    _ s)   = s
tokenContent (TClose   _ s)   = s
tokenContent (TSpace   _ n)   = replicate n ' '
tokenContent (TComment _ s)   = s
tokenContent (TUnknown _ s)   = s

-- | Text of token type, i.e., one of
--   @\"Word\"@, @\"Term\"@, @\"Open\"@, @\"Close\"@,
--   @\"Space\"@, @\"Comment\"@, or @\"Unknown\"@.
--
--   >>> tokenTypeText $ TWord 25 0 "flower"
--   "Word"
tokenTypeText :: Token -> String
tokenTypeText (TWord  _ _ _)  = "Word"
tokenTypeText (TShort _ _ _)  = "Short"
tokenTypeText (TTerm    _ _)  = "TermN"
tokenTypeText (TOpen    _ _)  = "Open"
tokenTypeText (TClose   _ _)  = "Close"
tokenTypeText (TSpace   _ _)  = "Space"
tokenTypeText (TComment _ _)  = "Comment"
tokenTypeText (TUnknown _ _)  = "Unknown"



-- ----------------------  Predicate

-- | Test the token is blank,
--   i.e., 'TComment' or 'TSpace'.
isBlankToken :: B.Pred Token
isBlankToken (TSpace _ _)    = True
isBlankToken (TComment _ _)  = True
isBlankToken _               = False

isShortToken :: B.Pred Token
isShortToken (TShort _ _ _)  = True
isShortToken _               = False

-- {-| Test the token is a term, i.e., 'TTerm'. -}
-- isTermToken :: Token -> Bool
-- isTermToken (TTerm _ _)     = True
-- isTermToken _               = False

-- | Check token is a 'TOpen' of the specific paren.
--
--   >>> let tok = TOpen 0 "(" in isOpenTokenOf "(" tok
--   True
--
--   >>> let tok = TOpen 0 "{" in isOpenTokenOf "(" tok
--   False
isOpenTokenOf :: String -> B.Pred Token
isOpenTokenOf p1 (TOpen _ p2) = p1 == p2
isOpenTokenOf _ _             = False

-- | Check token is a 'TClose' of the specific paren.
isCloseTokenOf :: String -> B.Pred Token
isCloseTokenOf p1 (TClose _ p2) = p1 == p2
isCloseTokenOf _ _              = False



-- ---------------------- Other functions

-- | Remove blank tokens.
sweepToken :: B.Map [Token]
sweepToken = filter (not . isBlankToken)

tokenIndent :: Token -> Int
tokenIndent (TSpace _ n) = n
tokenIndent _ = 0


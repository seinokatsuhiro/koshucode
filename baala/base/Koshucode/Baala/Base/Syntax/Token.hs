{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Syntax.Token
(
  -- * Token type
  Token (..),
  Termname,

  -- * Selectors
  tokenNumber,
  tokenContent,
  tokenTypeText,

  -- * Predicates
  isBlankToken,
  isOpenTokenOf,
  isCloseTokenOf,

  -- * Other function
  sweepToken,
) where

import Data.Generics (Data, Typeable)
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax.CodeLine



-- ----------------------  Token type

data Token
    = TWord    TokenNumber Int String  -- ^ Word.
                                       --   @Int@ represents quotation level, e.g.,
                                       --   0 for non-quoted,
                                       --   1 for single-quoted,
                                       --   2 for double-quoted.
    | TTerm    TokenNumber [Termname]  -- ^ Termname
    | TOpen    TokenNumber String      -- ^ Open paren
    | TClose   TokenNumber String      -- ^ Close paren
    | TSpace   TokenNumber Int         -- ^ /N/ space characters
    | TComment TokenNumber String      -- ^ Comment text
    | TUnknown TokenNumber String      -- ^ Unknown text
      deriving (Show, Eq, Ord, Data, Typeable)

instance Name Token where
    name (TTerm   _ ns) = concat ns
    name (TWord  _ _ s) = s
    name (TOpen    _ s) = s
    name (TClose   _ s) = s
    name (TComment _ s) = s
    name x = error $ "unknown name: " ++ show x

{-| Name of term,
    e.g., @\"\/file\"@ for the term @\/file@. -}
type Termname = String



-- ---------------------- Selector

tokenNumber :: Token -> TokenNumber
tokenNumber (TWord    n _ _)  = n
tokenNumber (TTerm    n _)    = n
tokenNumber (TOpen    n _)    = n
tokenNumber (TClose   n _)    = n
tokenNumber (TSpace   n _)    = n
tokenNumber (TComment n _)    = n
tokenNumber (TUnknown n _)    = n

tokenContent :: Token -> String
tokenContent (TWord  _ _ s)   = s
tokenContent (TTerm    _ s)   = concat s
tokenContent (TOpen    _ s)   = s
tokenContent (TClose   _ s)   = s
tokenContent (TSpace   _ n)   = replicate n ' '
tokenContent (TComment _ s)   = s
tokenContent (TUnknown _ s)   = s

{-| Text of token type, one of
    @\"Word\"@, @\"Term\"@, @\"Open\"@, @\"Close\"@,
    @\"Space\"@, @\"Comment\"@, or @\"Unknown\"@.

    >>> tokenTypeText $ TWord 1 0 "flower"
    "Word"
    -}
tokenTypeText :: Token -> String
tokenTypeText (TWord  _ _ _)  = "Word"
tokenTypeText (TTerm    _ _)  = "TermN"
tokenTypeText (TOpen    _ _)  = "Open"
tokenTypeText (TClose   _ _)  = "Close"
tokenTypeText (TSpace   _ _)  = "Space"
tokenTypeText (TComment _ _)  = "Comment"
tokenTypeText (TUnknown _ _)  = "Unknown"



-- ----------------------  Predicate

{-| Test the token is blank,
    i.e., 'TComment' or 'TSpace'. -}
isBlankToken :: Token -> Bool
isBlankToken (TSpace _ _)    = True
isBlankToken (TComment _ _)  = True
isBlankToken _               = False

-- {-| Test the token is a term, i.e., 'TTerm'. -}
-- isTermToken :: Token -> Bool
-- isTermToken (TTerm _ _)     = True
-- isTermToken _               = False

isOpenTokenOf :: String -> Token -> Bool
isOpenTokenOf p1 (TOpen _ p2) = p1 == p2
isOpenTokenOf _ _             = False

isCloseTokenOf :: String -> Token -> Bool
isCloseTokenOf p1 (TClose _ p2) = p1 == p2
isCloseTokenOf _ _              = False



-- ---------------------- Other functions

{-| Remove blank tokens. -}
sweepToken :: Map [Token]
sweepToken = filter (not . isBlankToken)

-- {-| Skip leading blank tokens. -}
-- sweepTokenLeft :: Map [Token]
-- sweepTokenLeft [] = []
-- sweepTokenLeft xxs@(x:xs)
--     | isBlankToken x = sweepTokenLeft xs
--     | otherwise = xxs


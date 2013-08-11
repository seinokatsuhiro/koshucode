{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

{-| Tokens -}

module Koshucode.Baala.Base.Syntax.Token
(
  -- * Token type
  -- $TokenType
  Termname,
  TokenNumber,
  Token (..),

  -- * Predicates
  isBlankToken,
  isTermToken,
  isOpenTokenOf,
  isCloseTokenOf,

  -- * Other functions
  tokenTypeText,
  tokenContent,
  tokenNumber,
  sweepToken,
  sweepLeft,
  divideByToken,
) where

import Data.Generics (Data, Typeable)

import Koshucode.Baala.Base.Prelude



-- ----------------------  Token type

{-| Termname. -}
type Termname = String

{-| Token number. -}
type TokenNumber = Int

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



-- ----------------------  Predicate

{-| Test the token is blank,
    i.e., 'TComment' or 'TSpace'. -}
isBlankToken :: Token -> Bool
isBlankToken (TSpace _ _)    = True
isBlankToken (TComment _ _)  = True
isBlankToken _               = False

{-| Test the token is a term, i.e., 'TTerm'. -}
isTermToken :: Token -> Bool
isTermToken (TTerm _ _)     = True
isTermToken _               = False

isOpenTokenOf :: String -> Token -> Bool
isOpenTokenOf p1 (TOpen _ p2) = p1 == p2
isOpenTokenOf _ _             = False

isCloseTokenOf :: String -> Token -> Bool
isCloseTokenOf p1 (TClose _ p2) = p1 == p2
isCloseTokenOf _ _              = False



-- ---------------------- Other functions

{-| Text of token type, one of
    @\"Word\"@, @\"Term\"@, @\"Open\"@, @\"Close\"@,
    @\"Space\"@, or @\"Comment\"@.

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

tokenContent :: Token -> String
tokenContent (TWord  _ _ s)   = s
tokenContent (TTerm    _ s)   = concat s
tokenContent (TOpen    _ s)   = s
tokenContent (TClose   _ s)   = s
tokenContent (TSpace   _ n)   = replicate n ' '
tokenContent (TComment _ s)   = s
tokenContent (TUnknown _ s)   = s

tokenNumber :: Token -> TokenNumber
tokenNumber (TWord    n _ _)  = n
tokenNumber (TTerm    n _)    = n
tokenNumber (TOpen    n _)    = n
tokenNumber (TClose   n _)    = n
tokenNumber (TSpace   n _)    = n
tokenNumber (TComment n _)    = n
tokenNumber (TUnknown n _)    = n

{-| Remove blank tokens. -}
sweepToken :: Map [Token]
sweepToken = filter (not . isBlankToken)

{-| Skip leading blank tokens. -}
sweepLeft :: Map [Token]
sweepLeft [] = []
sweepLeft xxs@(x:xs) | isBlankToken x = sweepLeft xs
                     | otherwise = xxs

{-| Divide token list by some word. -}
divideByToken :: String -> [Token] -> [[Token]]
divideByToken w = divideByP p where
    p (TWord _ 0 x) | w == x = True
    p _ = False



-- ----------------------
-- $TokenType
--
-- [Word]
--  Character sequence not including special characters,
--  e.g., @aa@, @r2d2@, @12.0@, etc.
--  There are four types of quotations.
--  (1) non-quoted word like @aa@,
--  (2) single-quoted word like @\'aa\'@,
--  (3) double-quoted word like @\"aa\"@.
--
-- [Term name]
--  Word beginning with slash, e.g., @\/aa@.
--  Term name like @\/a\/b@ is used for nested relation,
--  that means term @\/b@ in the relation of term @\/a@.
--
-- [Paren]
--  Open and closed parens.
--  (1) Round parens @(@ and @)@,
--  (2) Squared brackets @[@ and @]@.
--
-- [Space]
--  Space characters.
--
-- [Comment]
--  Texts from double asterisks (@**@) to
--  end of line are comments.
--  Quadruple asterisks (@****@) comments are
--  treated in 'Koshucode.Baala.Base.Section.Clause.CComment'.


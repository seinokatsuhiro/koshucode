{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokens in Koshucode

module Koshucode.Baala.Base.Syntax.Token
(
  -- * Token type
  -- $TokenType
  TNumber
, Token (..)

  -- * Predicates
, isBlankToken
, isTermToken
, isOpenTokenOf
, isCloseTokenOf

  -- * Other functions
, tokenTypeText
, tokenContent
, tokenNumber
, sweepToken
, sweepLeft
, hashText
) where

import Data.Generics (Data, Typeable)

import Koshucode.Baala.Base.Prelude



-- ----------------------  Token type

{-| Token number. -}
type TNumber = Int

data Token
    = TWord    TNumber Int String  -- ^ Word.
                                   --   @Int@ represents quotation level, e.g.,
                                   --   0 for non-quoted,
                                   --   1 for single-quoted,
                                   --   2 for double-quoted.
    | TTermN   TNumber [String]    -- ^ Term name
    | TTermP   TNumber [Int]       -- ^ Term position in particular relation.
                                   --   This is translate from 'TTermN'.
    | TOpen    TNumber String      -- ^ Open paren
    | TClose   TNumber String      -- ^ Close paren
    | TSpace   TNumber Int         -- ^ /N/ space characters
    | TComment TNumber String      -- ^ Comment text
      deriving (Show, Eq, Ord, Data, Typeable)

instance Name Token where
    name (TTermN  _ ns) = concat ns
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

{-| Test the token is a term,
    i.e., 'TTermN' or 'TTermP'. -}
isTermToken :: Token -> Bool
isTermToken (TTermN _ _)     = True
isTermToken (TTermP _ _)     = True
isTermToken _                = False

isOpenTokenOf :: String -> Token -> Bool
isOpenTokenOf p1 (TOpen _ p2) = p1 == p2
isOpenTokenOf _ _             = False

isCloseTokenOf :: String -> Token -> Bool
isCloseTokenOf p1 (TClose _ p2) = p1 == p2
isCloseTokenOf _ _              = False



-- ---------------------- Other functions

{-| Text of token type, one of
    @\"Word\"@, @\"TermN\"@, @\"TermP\"@, @\"Open\"@, @\"Close\"@,
    @\"Space\"@, or @\"Comment\"@.

    >>> tokenTypeText $ TWord 1 0 "flower"
    "Word"
    -}
tokenTypeText :: Token -> String
tokenTypeText (TWord _ _ _)  = "Word"
tokenTypeText (TTermN _ _)   = "TermN"
tokenTypeText (TTermP _ _)   = "TermP"
tokenTypeText (TOpen _ _)    = "Open"
tokenTypeText (TClose _ _)   = "Close"
tokenTypeText (TSpace _ _)   = "Space"
tokenTypeText (TComment _ _) = "Comment"

tokenContent :: Token -> String
tokenContent (TWord _ _ s)   = s
tokenContent (TTermN _ s)    = concat s
tokenContent (TTermP _ _)    = "#TermP"
tokenContent (TOpen _ s)     = s
tokenContent (TClose _ s)    = s
tokenContent (TSpace _ n)    = replicate n ' '
tokenContent (TComment _ s)  = s

tokenNumber :: Token -> TNumber
tokenNumber (TWord    n _ _) = n
tokenNumber (TTermN   n _)   = n
tokenNumber (TTermP   n _)   = n
tokenNumber (TOpen    n _)   = n
tokenNumber (TClose   n _)   = n
tokenNumber (TSpace   n _)   = n
tokenNumber (TComment n _)   = n

{-| Remove blank tokens. -}
sweepToken :: Map [Token]
sweepToken = filter (not . isBlankToken)

{-| Skip leading blank tokens. -}
sweepLeft :: Map [Token]
sweepLeft [] = []
sweepLeft xxs@(x:xs) | isBlankToken x = sweepLeft xs
                     | otherwise = xxs

hashText :: String -> Maybe String
hashText w =
    case w of
      "q"     -> Just "'"
      "qq"    -> Just "\""
      "cr"    -> Just "\r"
      "lf"    -> Just "\n"
      "crlf"  -> Just "\r\n"
      "tab"   -> Just "\t"
      "spc"   -> Just " "
      ""      -> Just "#"
      _       -> Nothing



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


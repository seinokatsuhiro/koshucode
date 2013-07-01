{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokens in Koshucode

module Koshucode.Baala.Base.Syntax.Token
(
  -- * Token type
  -- $TokenType
  Token (..)
, tokenTypeText
, tokenContent

  -- * Predicates
, isBlankToken
, isTermToken

  -- * Source lines
, SourceLine (..)

  -- * Othre functions
, sweepToken
, sweepLeft
, linesCrLf
) where

import Data.Generics (Data, Typeable)

import Koshucode.Baala.Base.Prelude



-- ----------------------  Token type

data Token
    = TWord Int String   -- ^ Word.
                         --   @Int@ represents quotation level, e.g.,
                         --   0 for non-quoted,
                         --   1 for single-quoted,
                         --   2 for double-quoted.
    | TTermN   [String]  -- ^ Term name
    | TTermP   [Int]     -- ^ Term position in particular relation.
                         --   This is translate from 'TermN'.
    | TOpen    String    -- ^ Open paren
    | TClose   String    -- ^ Close paren
    | TSpace   Int       -- ^ /N/ space characters
    | TComment String    -- ^ Comment text
      deriving (Show, Eq, Ord, Data, Typeable)

instance Name Token where
    name (TTermN  ns) = concat ns
    name (TWord  _ s) = s
    name (TOpen    s) = s
    name (TClose   s) = s
    name (TComment s) = s
    name x = error $ "unknown name: " ++ show x

{-| Text of token type, one of
    @\"Word\"@, @\"TermN\"@, @\"TermP\"@, @\"Open\"@, @\"Close\"@,
    @\"Space\"@, @\"Comment\"@, or @\"Line@\".

    >>> tokenTypeText $ Word 0 "flower"
    "Word"
    -}
tokenTypeText :: Token -> String
tokenTypeText (TWord _ _)  = "Word"
tokenTypeText (TTermN _)   = "TermN"
tokenTypeText (TTermP _)   = "TermP"
tokenTypeText (TOpen _)    = "Open"
tokenTypeText (TClose _)   = "Close"
tokenTypeText (TSpace _)   = "Space"
tokenTypeText (TComment _) = "Comment"

tokenContent :: Token -> String
tokenContent (TWord _ s)   = s
tokenContent (TTermN s)    = concat s
tokenContent (TTermP _)    = "#TermP"
tokenContent (TOpen s)     = s
tokenContent (TClose s)    = s
tokenContent (TSpace n)    = replicate n ' '
tokenContent (TComment s)  = s



-- ----------------------  Predicate

{-| Test the token is blank, i.e.,
    'Comment', 'Line', or 'Space'. -}
isBlankToken :: Token -> Bool
isBlankToken (TSpace _)    = True
isBlankToken (TComment _)  = True
isBlankToken _             = False

{-| Test the token is a term,
    i.e., 'TermN' or 'TermP'. -}
isTermToken :: Token -> Bool
isTermToken (TTermN _)     = True
isTermToken (TTermP _)     = True
isTermToken _              = False



-- ----------------------  Source line

{-| Source line information.
    It consists of (1) line number,
    (2) line content, and (3) tokens from line. -}
data SourceLine = SourceLine
    { sourceLineNumber  :: Int
    , sourceLineContent :: String
    , sourceLineTokens  :: [Token]
    } deriving (Show, Eq, Ord, Data, Typeable)

instance Pretty SourceLine where
    doc (SourceLine _ line _) = text line



-- ---------------------- Other functions

{-| Remove blank tokens. -}
sweepToken :: Map [Token]
sweepToken = filter (not . isBlankToken)

{-| Skip leading blank tokens. -}
sweepLeft :: Map [Token]
sweepLeft [] = []
sweepLeft xxs@(x:xs) | isBlankToken x = sweepLeft xs
                     | otherwise = xxs

{-| Split string into lines.
    The result strings do not contain
    carriage returns (@\\r@)
    and line feeds (@\\n@). -}
linesCrLf :: String -> [String]
linesCrLf "" = []
linesCrLf s = ln : nextLine s2 where
    (ln, s2) = break (`elem` "\r\n") s
    nextLine ('\r' : s3) = nextLine s3
    nextLine ('\n' : s3) = nextLine s3
    nextLine s3 = linesCrLf s3


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
--  Text from double asterisks (@**@) to
--  end of line is a comment.


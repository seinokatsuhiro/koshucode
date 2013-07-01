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
, isLineToken
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
    = Word Int String   -- ^ Word.
                        --   @Int@ represents quotation level, e.g.,
                        --   0 for non-quoted,
                        --   1 for single-quoted,
                        --   2 for double-quoted.
    | TermN   [String]  -- ^ Term name
    | TermP   [Int]     -- ^ Term position in particular relation.
                        --   This is translate from 'TermN'.
    | Open    String    -- ^ Open paren
    | Close   String    -- ^ Close paren
    | Space   Int       -- ^ /N/ space characters
    | Comment String    -- ^ Comment text
    | Line SourceLine   -- ^ Source infomation
      deriving (Show, Eq, Ord, Data, Typeable)

instance Name Token where
    name (TermN  ns) = concat ns
    name (Word  _ s) = s
    name (Open    s) = s
    name (Close   s) = s
    name (Comment s) = s
    name x = error $ "unknown name: " ++ show x

{-| Text of token type, one of
    @\"Word\"@, @\"TermN\"@, @\"TermP\"@, @\"Open\"@, @\"Close\"@,
    @\"Space\"@, @\"Comment\"@, or @\"Line@\".

    >>> tokenTypeText $ Word 0 "flower"
    "Word"
    -}
tokenTypeText :: Token -> String
tokenTypeText (Word _ _)  = "Word"
tokenTypeText (TermN _)   = "TermN"
tokenTypeText (TermP _)   = "TermP"
tokenTypeText (Open _)    = "Open"
tokenTypeText (Close _)   = "Close"
tokenTypeText (Space _)   = "Space"
tokenTypeText (Comment _) = "Comment"
tokenTypeText (Line _)    = "Line"

tokenContent :: Token -> String
tokenContent (Word _ s)  = s
tokenContent (TermN s)   = concat s
tokenContent (TermP _)   = "#TermP"
tokenContent (Open s)    = s
tokenContent (Close s)   = s
tokenContent (Space n)   = replicate n ' '
tokenContent (Comment s) = s
tokenContent (Line (SourceLine _ s _)) = s




-- ----------------------  Predicate

{-| Test the token is blank, i.e.,
    'Comment', 'Line', or 'Space'. -}
isBlankToken :: Token -> Bool
isBlankToken (Space _)    = True
isBlankToken (Comment _)  = True
isBlankToken (Line _)     = True
isBlankToken _            = False

{-| Test the token is a term,
    i.e., 'TermN' or 'TermP'. -}
isTermToken :: Token -> Bool
isTermToken (TermN _)     = True
isTermToken (TermP _)     = True
isTermToken _             = False

{-| Test the token is a line,
    i.e., 'Line'. -}
isLineToken :: Token -> Bool
isLineToken (Line _)      = True
isLineToken _             = False



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
--
-- [Line]
--  Source line.


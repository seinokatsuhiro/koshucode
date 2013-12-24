{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

{-| This module provides a container for tokens.
    No tokens in Koshucode are in a extent of multiple lines.
    'CodeLine' includes whole tokens in a line.
    You can represent tokenized source code
    as a list of 'CodeLine'. -}

module Koshucode.Baala.Base.Syntax.Code
( CodeLine (..),
  lineNumberContent,
  CodeClause (..),
  NextToken,
  TokenNumber,
  codeLines,
) where

import qualified Data.Generics as G
import qualified Koshucode.Baala.Base.Prelude     as B
import qualified Koshucode.Baala.Base.Syntax.Line as B



-- ----------------------  CodeLine

{-| Tokens in line. -}
data CodeLine a = CodeLine
    { lineNumber  :: B.LineNumber  -- ^ Line number, from 1.
    , lineContent :: String        -- ^ Line content without newline.
    , lineTokens  :: [a]           -- ^ Tokens in the line.
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

lineTokensCount :: CodeLine a -> Int
lineTokensCount = length . lineTokens

lineNumberContent :: CodeLine a -> String
lineNumberContent c = show (lineNumber c) ++ " " ++ (lineContent c)

{-| Tokens in clause. -}
data CodeClause a = CodeClause
    { clauseTokens    :: [a]           -- ^ Source tokens of clause
    , clauseLines     :: [CodeLine a]  -- ^ Source lines of clause
    } deriving (Show, G.Data, G.Typeable)

instance B.Pretty (CodeLine a) where
    doc (CodeLine _ line _) = B.doc line

{-| Type of function that splits a next token from string.
    Tokens can includes 'TokenNumber'. -}
type NextToken a
    =  B.NumberedLine
    -> TokenNumber   -- ^ Token number, from 1
    -> String        -- ^ Source text
    -> (a, String)   -- ^ Token and rest of text

{-| Token number, from 1. -}
type TokenNumber = Int

{-| Split source text into 'CodeLine' list.

    1. Split source text into lines by line delimiters
       (carriage return @\\r@ or line feed @\\n@).

    2. Numbering lines from 1.
       Internally, this is represented as
       a list of pairs @(@'LineNumber'@,@ 'String'@)@.

    3. Tokenize each lines,
       and put tokens together in 'CodeLine'.
  -}
codeLines
    :: NextToken a     -- ^ Token splitter
    -> String          -- ^ Source text
    -> [CodeLine a]    -- ^ Token list per lines
codeLines nextToken = codeLinesBy $ codeLine nextToken

type MakeCodeLine a
    = TokenNumber -> B.NumberedLine -> CodeLine a

codeLinesBy :: MakeCodeLine a -> String -> [CodeLine a]
codeLinesBy mkCline = loop 1 . B.linesCrlfNumbered where
    loop _ [] = []
    loop tno (line : rest) =
        case mkCline tno line of
          cline -> let delta = lineTokensCount cline
                   in cline : loop (tno + delta) rest

codeLine :: NextToken a -> MakeCodeLine a
codeLine nextToken tno line@(lno, text) = CodeLine lno text toks where
    toks = B.gatherWith (nextToken line) [tno ..] text


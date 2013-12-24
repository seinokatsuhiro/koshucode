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
    -> String        -- ^ Source text
    -> (a, String)   -- ^ Token and rest of text

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

type MakeCodeLine a = B.NumberedLine -> CodeLine a

codeLinesBy :: MakeCodeLine a -> String -> [CodeLine a]
codeLinesBy mkCline = loop . B.linesCrlfNumbered where
    loop [] = []
    loop (line : rest) = mkCline line : loop rest

codeLine :: NextToken a -> MakeCodeLine a
codeLine nextToken line@(lno, text) =
    let toks = B.gather (nextToken line) text
    in CodeLine lno text toks


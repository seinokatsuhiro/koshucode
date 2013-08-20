{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

{-| This module provides a container for tokens.
    No tokens in Koshucode are in a extent of multiple lines.
    'CodeLine' includes whole tokens in a line.
    You can represent tokenized source code
    as a list of 'CodeLine'. -}

module Koshucode.Baala.Base.Syntax.CodeLine
( CodeLine (..),
  NextToken,
  TokenNumber,
  codeLines,
) where

import qualified Data.Generics as G
import Koshucode.Baala.Base.Prelude



-- ----------------------  Codeline

{-| Tokens per line. -}
data CodeLine a = CodeLine
    { codeLineNumber  :: LineNumber -- ^ Line number, from 1.
    , codeLineContent :: String     -- ^ Line content without newline.
    , codeLineTokens  :: [a]        -- ^ Tokens in the line.
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance Pretty (CodeLine a) where
    doc (CodeLine _ line _) = doc line

{-| Type of function that splits a next token from string.
    Tokens can includes 'TokenNumber'. -}
type NextToken a
    =  TokenNumber   -- ^ Token number, from 1
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
codeLines next = codeLinesBy $ codeLine next

codeLinesBy
    :: (LineNumber -> TokenNumber -> String -> CodeLine a)
    -> String -> [CodeLine a]
codeLinesBy f = loop 1 . linesCrlfNumbered where
    loop _ [] = []
    loop tno ((lno, ln) : ps) =
        case f tno lno ln of
          cline -> let delta = length $ codeLineTokens cline
                   in cline : loop (tno + delta) ps

codeLine
    :: NextToken a    -- ^ Token splitter
    -> TokenNumber    -- ^ Token number
    -> LineNumber     -- ^ Line number
    -> String         -- ^ Line content
    -> CodeLine a     -- ^ Result 'CodeLine'
codeLine next tno lno ln = CodeLine lno ln toks where
    toks = gatherWith next [tno ..] ln



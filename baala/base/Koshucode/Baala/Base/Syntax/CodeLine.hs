{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

{-| Tokenizer of koshucode. -}

module Koshucode.Baala.Base.Syntax.CodeLine
( CodeLine (..),
  NextToken,
  codeLines,
) where

import Data.Generics (Data, Typeable)
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax.Token



-- ----------------------  Codeline

data CodeLine = CodeLine
    { codeLineNumber  :: LineNumber -- ^ Line number, from 1.
    , codeLineContent :: String     -- ^ Line content without newline.
    , codeLineTokens  :: [Token]    -- ^ Tokens in the line.
    } deriving (Show, Eq, Ord, Data, Typeable)

instance Pretty CodeLine where
    doc (CodeLine _ line _) = text line

type NextToken = TokenNumber -> String -> (Token, String)

{-| Split source text into 'CodeLine' list.

    1. Split source code into lines by line delimiters
       (carriage return @\\r@ or line feed @\\n@)

    2. Numbering line numbers.
       Internally, this is represented as
       a list of pairs (/line#/, /string/).

    3. Tokenize each lines.
       This is represented as a list of
       'CodeLine' /line#/ /string/ /tokens/.
  -}
codeLines
    :: NextToken    -- ^ Token splitter
    -> String       -- ^ Source text.
    -> [CodeLine]   -- ^ Token list per lines.
codeLines next = codeLinesBy $ codeLine next

codeLinesBy
    :: (TokenNumber -> (LineNumber, String) -> CodeLine)
    -> String -> [CodeLine]
codeLinesBy f = loop 1 . linesCrlfNumbered where
    loop _ [] = []
    loop tno (p : ps) =
        case f tno p of
          cline -> let tno' = tno + (length $ codeLineTokens cline)
                   in cline : loop tno' ps

codeLine
    :: NextToken             -- ^ Token splitter
    -> TokenNumber           -- ^ Token number
    -> (LineNumber, String)  -- ^ Line number and content
    -> CodeLine              -- ^ Result 'CodeLine'
codeLine next tno (lno, ln) = CodeLine lno ln toks where
    toks = gatherWith next [tno ..] ln



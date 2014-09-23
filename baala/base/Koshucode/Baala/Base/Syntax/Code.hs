{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | This module provides a container for tokens.
--   No tokens in Koshucode are in a extent of multiple lines.
--   'CodeLine' includes whole tokens in a line.
--   You can represent tokenized source code
--   as a list of 'CodeLine'.

module Koshucode.Baala.Base.Syntax.Code
( -- * CodeLine
  CodeLine (..),
  CodeClause (..),
  lineIndentPair,
  splitClause,

  -- * CodeRoll
  CodeRoll (..),
  codeRollUp,
  codeUpdate, codeChange
) where

import qualified Data.Generics                    as G
import qualified Koshucode.Baala.Base.Abort       as B
import qualified Koshucode.Baala.Base.Prelude     as B
import qualified Koshucode.Baala.Base.Text        as B
import qualified Koshucode.Baala.Base.Syntax.Line as B
import qualified Koshucode.Baala.Base.Message     as Msg



-- ----------------------  CodeLine

-- | Tokens in line.
data CodeLine a = CodeLine
    { lineNumber  :: B.LineNumber  -- ^ Line number, from 1.
    , lineContent :: String        -- ^ Line content without newline.
    , lineTokens  :: [a]           -- ^ Tokens in the line.
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Tokens in clause.
data CodeClause a = CodeClause
    { clauseLines     :: [CodeLine a]  -- ^ Source lines of clause
    , clauseTokens    :: [a]           -- ^ Source tokens of clause
    } deriving (Show, G.Data, G.Typeable)

instance B.Write (CodeLine a) where
    write sh (CodeLine _ line _) = B.write sh line

instance (B.CodePtr a) => B.CodePtr (CodeLine a) where
    codePts (CodeLine _ _ ts) = B.codePts $ head ts

instance (B.CodePtr a) => B.CodePtr (CodeClause a) where
    codePts (CodeClause _ ts) = B.codePts $ head ts

lineIndentPair :: (a -> Int) -> CodeLine a -> (Int, CodeLine a)
lineIndentPair ind ln@(CodeLine _ _ (tk : _)) = (ind tk, ln)
lineIndentPair _   ln@(CodeLine _ _ [])       = (0, ln)

splitClause :: B.Gather [(Int, a)] [a]
splitClause = first where
    first    ((i, x) : xs)            = B.cons1 x $ continue i xs
    first    []                       = ([], [])
    continue i ((n, x) : xs) | n > i  = B.cons1 x $ continue i xs
    continue _ xs                     = ([], xs)


-- ----------------------  CodeRoll

data CodeRoll a =
    CodeRoll { codeMap     :: B.AbMap (CodeRoll a)
             , codeInputPt :: B.CodePt
             , codeInput   :: String
             , codeOutput  :: [a]
             }

-- | Split source text into 'CodeLine' list.
--
--   1. Split source text into lines by line delimiters
--      (carriage return @\\r@ or line feed @\\n@).
--
--   2. Numbering lines from 1.
--      Internally, this is represented as
--      a list of pairs @(@'B.LineNumber'@,@ 'String'@)@.
--
--   3. Tokenize each lines,
--      and put tokens together in 'CodeLine'.
--
codeRollUp :: B.AbMap (CodeRoll a) -> B.Resource -> String -> B.Ab [CodeLine a]
codeRollUp f res = loop (CodeRoll f cp "" []) . B.linesCrlfNumbered where
    cp    = B.codeZero { B.codeResource = res }

    loop _ [] = Right []
    loop r ((num, line) : ls) =
       do let cp' = setLine num line cp
          r' <- Msg.abToken [cp'] $ codeRoll $ setRoll cp' line r
          let toks = reverse $ codeOutput r'
          ls' <- loop r' ls
          Right $ CodeLine num line toks : ls'

codeRoll :: B.AbMap (CodeRoll a)
codeRoll r@CodeRoll { codeMap = f, codeInputPt = cp, codeInput = input }
    | null input = Right r
    | otherwise  = codeRoll =<< f r { codeInputPt = setText input cp }

codeUpdate :: String -> a -> B.Map (CodeRoll a)
codeUpdate cs tok r = r { codeInput = cs, codeOutput = tok : codeOutput r }

codeChange :: B.AbMap (CodeRoll a) -> B.Map (CodeRoll a)
codeChange f r = r { codeMap = f }

setLine :: Int -> String -> B.Map B.CodePt
setLine num line cp =
    cp { B.codeLineNumber = num
       , B.codeLineText   = line
       , B.codeText       = line }

setText :: String -> B.Map B.CodePt
setText text cp = cp { B.codeText = text }

setRoll :: B.CodePt -> String -> CodeRoll a -> CodeRoll a
setRoll cp line r = r { codeInputPt = cp
                      , codeInput   = line
                      , codeOutput  = [] }


{-# OPTIONS_GHC -Wall #-}

-- | This module provides a container for tokens.
--   No tokens in Koshucode are in a extent of multiple lines.
--   'CodeLine' includes whole tokens in a line.
--   You can represent tokenized source code
--   as a list of 'CodeLine'.

module Koshucode.Baala.Base.Code.Clause
  ( -- * CodeClause
    CodeClause (..),
    codeClauseEmpty,  
    splitClause,
  ) where

import qualified Koshucode.Baala.Base.IO              as B
import qualified Koshucode.Baala.Base.List            as B
import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Code.Line       as B


-- ----------------------  CodeClause

-- | Tokens in clause.
data CodeClause a = CodeClause
    { clauseLines     :: [B.CodeLine a]  -- ^ Source lines of clause
    , clauseTokens    :: [a]             -- ^ Source tokens of clause
    } deriving (Show)

instance (B.CodePtr a) => B.CodePtr (CodeClause a) where
    codePtList (CodeClause _ ts) = B.codePtList $ head ts

-- | Clause with no tokens.
codeClauseEmpty :: CodeClause a
codeClauseEmpty = CodeClause [] []

-- | Split lines into clause based on indent size.
--
--   >>> splitClause [(0, "a"), (2, "b"), (0, "c"), (1, "d")]
--   (["a","b"], [(0,"c"),(1,"d")])
--
splitClause :: B.Gather [(B.IndentSize, a)] [a]
splitClause = first where
    first    ((i, x) : xs)            = B.consFst x $ continue i xs
    first    []                       = ([], [])
    continue i ((n, x) : xs) | n > i  = B.consFst x $ continue i xs
    continue _ xs                     = ([], xs)


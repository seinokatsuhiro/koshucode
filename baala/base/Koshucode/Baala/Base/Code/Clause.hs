{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-- | This module provides a container for tokens.
--   No tokens in Koshucode are in a extent of multiple lines.
--   'CodeLine' includes whole tokens in a line.
--   You can represent tokenized source code
--   as a list of 'CodeLine'.

module Koshucode.Baala.Base.Code.Clause
  ( -- * CodeClause
    CodeClause (..),
    splitClause,
  ) where

import qualified Koshucode.Baala.Base.Abort           as B
import qualified Koshucode.Baala.Base.List            as B
import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Code.Line       as B

-- | Tokens in clause.
data CodeClause k t = CodeClause
    { clauseLines     :: [B.CodeLine k t]  -- ^ Source lines of clause
    , clauseTokens    :: [k t]             -- ^ Source tokens of clause
    } deriving (Show)

instance (B.GetCodePos (k t)) => B.GetCodePos (CodeClause k t) where
    getCPs (CodeClause _ ts) = B.getCPs $ head ts

-- | No lines, no tokens.
instance B.Default (CodeClause t a) where
    def = CodeClause [] []

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


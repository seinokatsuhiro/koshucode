{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Koshucode.Baala.Syntax.Token.TokenClause
  ( TokenClause,
    tokenClauses,
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Token.TokenLine as S
import qualified Koshucode.Baala.Syntax.Token.Utility   as S

type TokenClause = B.CodeClause S.Token

-- | Convert token lines into token clauses
tokenClauses :: [S.TokenLine] -> [TokenClause]
tokenClauses = map clause . split where
    clause ls = B.CodeClause ls $ tokens ls

    tokens :: [S.TokenLine] -> [S.Token]
    tokens = concatMap $ S.sweepToken . B.lineTokens

    split :: [S.TokenLine] -> [[S.TokenLine]]
    split = B.gather B.splitClause . map indent . B.omit blank

    blank :: S.TokenLine -> Bool
    blank = all S.isBlankToken . B.lineTokens

    indent :: S.TokenLine -> (Int, S.TokenLine)
    indent = B.lineIndentPair tokenIndent

tokenIndent :: S.Token -> Int
tokenIndent (S.TSpace _ n) = n
tokenIndent _ = 0

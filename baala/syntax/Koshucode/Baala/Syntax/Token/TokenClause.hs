{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Koshucode.Baala.Syntax.Token.TokenClause
  ( TokenClause,
    tokenClauses,
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Token.TokenLine as S

type TokenClause = B.CodeClause S.Token

instance B.Write TokenClause where
    writeDocWith = docTokenClause

docTokenClause :: B.Shortener -> TokenClause -> B.Doc
docTokenClause sh (B.CodeClause ls toks) = d where
    d      = B.docHang k 2 $ B.docv [dls, dtoks]
    k      = B.writeDocWith sh "TokenClause"
    dtoks  = labeled toks "[Token]"
    dls    = labeled ls   "[TokenLine]"
    labeled xs name = B.docHang (label xs name) 2 $ B.docv xs
    label   xs name = B.writeH sh [ B.writeDocWith sh name
                                  , B.writeDocWith sh $ length xs
                                  , B.writeDocWith sh "elements" ]

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

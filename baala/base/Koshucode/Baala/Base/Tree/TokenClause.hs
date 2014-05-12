{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Koshucode.Baala.Base.Tree.TokenClause
( TokenClause,
  tokenClauses,
) where

import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Syntax          as B
import qualified Koshucode.Baala.Base.Text            as B
import qualified Koshucode.Baala.Base.Token.Token     as B
import qualified Koshucode.Baala.Base.Tree.TokenLine as B



type TokenClause = B.CodeClause B.Token

instance B.Pretty TokenClause where
    doc = docTokenClause

docTokenClause :: TokenClause -> B.Doc
docTokenClause (B.CodeClause ls toks) = d where
    d      = B.docHang k 2 $ B.docv [dls, dtoks]
    k      = B.doc "TokenClause"
    dtoks  = labeled toks "[Token]"
    dls    = labeled ls   "[TokenLine]"
    labeled xs name = B.docHang (label xs name) 2 $ B.docv xs
    label   xs name = B.doch [ B.doc name
                             , B.doc $ length xs, B.doc "elements" ]

-- | Convert token lines into token clauses
tokenClauses :: [B.TokenLine] -> [TokenClause]
tokenClauses = map clause . split where
    clause ls = B.CodeClause ls $ tokens ls
    tokens ls = concatMap (B.sweepToken . B.lineTokens) ls

    split :: [B.TokenLine] -> [[B.TokenLine]]
    split = B.gather B.splitClause . map indent . sweep

    sweep :: B.Map [B.TokenLine]
    sweep = filter (not . blank)

    blank :: B.TokenLine -> Bool
    blank = all B.isBlankToken . B.lineTokens

    indent :: B.TokenLine -> (Int, B.TokenLine)
    indent = B.indentLineBy B.tokenIndent


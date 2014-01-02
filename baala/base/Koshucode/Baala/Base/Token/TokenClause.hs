{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Koshucode.Baala.Base.Token.TokenClause
( TokenClause,
  tokenClauses,
) where

import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Syntax          as B
import qualified Koshucode.Baala.Base.Token.Token     as B
import qualified Koshucode.Baala.Base.Token.TokenLine as B



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

{-| Convert token lines into token clauses -}
tokenClauses :: [B.TokenLine] -> [TokenClause]
tokenClauses = map clause . split where
    clause ls = B.CodeClause ls $ concatMap B.lineTokens ls

    split :: [B.TokenLine] -> [[B.TokenLine]]
    split = B.gather B.splitClause . map indentLine . sweep

    sweep :: B.Map [B.TokenLine]
    sweep = filter (not . isBlankLine)

indentLine :: B.TokenLine -> (Int, B.TokenLine)
indentLine = B.indentLineBy B.tokenIndent

isBlankLine :: B.TokenLine -> Bool
isBlankLine = all B.isBlankToken . B.lineTokens


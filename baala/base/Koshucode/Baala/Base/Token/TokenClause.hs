{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Koshucode.Baala.Base.Token.TokenClause
( TokenClause,
  tokenClauses,
) where

import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Syntax          as B
import qualified Koshucode.Baala.Base.Text            as B
import qualified Koshucode.Baala.Base.Token.Token     as B
import qualified Koshucode.Baala.Base.Token.TokenLine as B

type TokenClause = B.CodeClause B.Token

instance B.ShortDoc TokenClause where
    shortDoc = docTokenClause

docTokenClause :: [B.ShortDef] -> TokenClause -> B.Doc
docTokenClause sh (B.CodeClause ls toks) = d where
    d      = B.docHang k 2 $ B.docv [dls, dtoks]
    k      = B.shortDoc sh "TokenClause"
    dtoks  = labeled toks "[Token]"
    dls    = labeled ls   "[TokenLine]"
    labeled xs name = B.docHang (label xs name) 2 $ B.docv xs
    label   xs name = B.shortDocH sh [ B.shortDoc sh name
                                     , B.shortDoc sh $ length xs
                                     , B.shortDoc sh "elements" ]

-- | Convert token lines into token clauses
tokenClauses :: [B.TokenLine] -> [TokenClause]
tokenClauses = map clause . split where
    clause ls = B.CodeClause ls $ tokens ls
    tokens ls = concatMap (B.sweepToken . B.lineTokens) ls

    split :: [B.TokenLine] -> [[B.TokenLine]]
    split = B.gather B.splitClause . map indent . sweep

    sweep :: B.Map [B.TokenLine]
    sweep = B.omit blank

    blank :: B.TokenLine -> Bool
    blank = all B.isBlankToken . B.lineTokens

    indent :: B.TokenLine -> (Int, B.TokenLine)
    indent = B.indentLineBy B.tokenIndent

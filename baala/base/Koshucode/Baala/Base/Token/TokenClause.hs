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

instance B.Write TokenClause where
    writeDocWith = docTokenClause

docTokenClause :: B.StringMap -> TokenClause -> B.Doc
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
tokenClauses :: [B.TokenLine] -> [TokenClause]
tokenClauses = map clause . split where
    clause ls = B.CodeClause ls $ tokens ls

    tokens :: [B.TokenLine] -> [B.Token]
    tokens = concatMap $ B.sweepToken . B.lineTokens

    split :: [B.TokenLine] -> [[B.TokenLine]]
    split = B.gather B.splitClause . map indent . B.omit blank

    blank :: B.TokenLine -> Bool
    blank = all B.isBlankToken . B.lineTokens

    indent :: B.TokenLine -> (Int, B.TokenLine)
    indent = B.lineIndentPair tokenIndent

tokenIndent :: B.Token -> Int
tokenIndent (B.TSpace _ n) = n
tokenIndent _ = 0

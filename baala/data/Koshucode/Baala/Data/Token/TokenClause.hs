{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Koshucode.Baala.Data.Token.TokenClause
  ( TokenClause,
    tokenClauses,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Data.Token.Token     as D
import qualified Koshucode.Baala.Data.Token.TokenLine as D

type TokenClause = B.CodeClause D.Token

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
tokenClauses :: [D.TokenLine] -> [TokenClause]
tokenClauses = map clause . split where
    clause ls = B.CodeClause ls $ tokens ls

    tokens :: [D.TokenLine] -> [D.Token]
    tokens = concatMap $ D.sweepToken . B.lineTokens

    split :: [D.TokenLine] -> [[D.TokenLine]]
    split = B.gather B.splitClause . map indent . B.omit blank

    blank :: D.TokenLine -> Bool
    blank = all D.isBlankToken . B.lineTokens

    indent :: D.TokenLine -> (Int, D.TokenLine)
    indent = B.lineIndentPair tokenIndent

tokenIndent :: D.Token -> Int
tokenIndent (D.TSpace _ n) = n
tokenIndent _ = 0

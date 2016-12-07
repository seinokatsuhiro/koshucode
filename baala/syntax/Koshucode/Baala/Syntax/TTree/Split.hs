{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Split token trees.

module Koshucode.Baala.Syntax.TTree.Split
  ( -- * Split and divide
    splitTokensBy, splitTreesBy,
    divideTreesBy,
    divideTreesByBar, divideTreesByColon, divideTreesByEqual,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.TTree.TokenTree  as S
import qualified Koshucode.Baala.Syntax.Token.Pattern    as P
import qualified Koshucode.Baala.Syntax.TTree.Pattern    as P

-- | Split token list by unquoted word.
--   If token list contains the word,
--   pair of /before-list/, /the-word/ and /after-list/ is returned.
--   If does not contains the word,
--   original token list is returned.
--
--   >>> let Right t = S.toks "b c"
--   >>> splitTokensBy (== "|") t
--   Left [ TText  CodePos {..} TextRaw "b"
--        , TSpace CodePos {..} 1
--        , TText  CodePos {..} TextRaw "c" ]
--
--   >>> let Right t = S.toks "a | b | c"
--   >>> splitTokensBy (== "|") t
--   Right ( [ TText CodePos {..} TextRaw "a", TSpace CodePos {..} 1]
--         , TText CodePos {..} TextRaw "|"
--         , [ TText  CodePos {..} TextRaw "b"
--           , TSpace CodePos {..} 1
--           , TText  CodePos {..} TextRaw "|"
--           , TText  CodePos {..} TextRaw "c" ] )
--
splitTokensBy :: O.Test String -> B.SplitList3e S.Token
splitTokensBy p = B.splitBy p2 where
    p2 (P.TRaw x) = p x
    p2 _          = False

raw :: O.Test String -> S.TTree -> Bool
raw p (P.LRaw w) = p w
raw _ _          = False

-- | Split token trees by quoteless token of given string.
splitTreesBy :: O.Test String -> B.SplitList3e S.TTree
splitTreesBy = B.splitBy . raw

-- | Divide token trees by quoteless token of given string.
divideTreesBy :: O.Test String -> [S.TTree] -> [[S.TTree]]
divideTreesBy = B.divideBy . raw

-- | Divide token trees by vertical bar @\"|\"@.
divideTreesByBar :: [S.TTree] -> [[S.TTree]]
divideTreesByBar = divideTreesBy (== "|")

-- | Divide token trees by colon @\":\"@.
divideTreesByColon :: [S.TTree] -> [[S.TTree]]
divideTreesByColon = divideTreesBy (== ":")

-- | Divide token trees by equal sign @\"=\"@.
divideTreesByEqual :: [S.TTree] -> [[S.TTree]]
divideTreesByEqual = divideTreesBy (== "=")


{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Split token trees.

module Koshucode.Baala.Syntax.Tree.Split
  ( -- * Split
    splitTokensBy, splitTreesBy,
    -- * Divide
    divideTreesBy,
    divideTreesByBar, divideTreesByColon, divideTreesByEqual,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.Tree.Parse       as S
import qualified Koshucode.Baala.Syntax.Token.Pattern    as P
import qualified Koshucode.Baala.Syntax.Tree.Pattern     as P

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

raw :: O.Test String -> S.Tree -> Bool
raw p (P.LRaw w) = p w
raw _ _          = False

-- | Split token trees by quoteless token of given string.
splitTreesBy :: O.Test String -> B.SplitList3e S.Tree
splitTreesBy = B.splitBy . raw

-- | Divide token trees by quoteless token of given string.
divideTreesBy :: O.Test String -> [S.Tree] -> [[S.Tree]]
divideTreesBy = B.divideBy . raw

-- | Divide token trees by vertical bar @\"|\"@.
divideTreesByBar :: [S.Tree] -> [[S.Tree]]
divideTreesByBar = divideTreesBy (== "|")

-- | Divide token trees by colon @\":\"@.
divideTreesByColon :: [S.Tree] -> [[S.Tree]]
divideTreesByColon = divideTreesBy (== ":")

-- | Divide token trees by equal sign @\"=\"@.
divideTreesByEqual :: [S.Tree] -> [[S.Tree]]
divideTreesByEqual = divideTreesBy (== "=")


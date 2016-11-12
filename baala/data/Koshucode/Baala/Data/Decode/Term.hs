{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decode term name.

module Koshucode.Baala.Data.Decode.Term
  ( -- * Term name
    treeFlatName,
    treesFlatNames,
    treesTerms,
    treesTerms1,
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Data.Decode.Message   as Msg

import Koshucode.Baala.Syntax.TTree.Pattern


-- ----------------------  Term

-- | Read flat term name from token tree.
--   If the token tree contains nested term name, this function failed.
--
--   >>> S.tt1 "/a" >>= treeFlatName
--   Right "a"
--
--   >>> S.tt1 "+/a" >>= treeFlatName
--   Right "a"
--
--   >>> S.tt1 "/a/x" >>= treeFlatName
--   Left ...
--
treeFlatName :: S.TTree -> B.Ab S.TermName
treeFlatName (L (S.TTermN _ _ n))  = Right n
--treeFlatName (L (S.TTerm _ _ [n])) = Right n
treeFlatName (L t)                 = Msg.reqFlatName t
treeFlatName _                     = Msg.reqTermName

-- | Read flat term names.
--
--   >>> S.tt "/a /b" >>= treesFlatNames
--   Right ["a","b"]
--
treesFlatNames :: [S.TTree] -> B.Ab [S.TermName]
treesFlatNames = mapM treeFlatName

-- | Read list of named token trees from token trees.
--
--   >>> S.tt "/a 'A3 /b 10" >>= treesTerms
--   Right [("a", [TreeL ...]),
--          ("b", [TreeL ...])]
--
treesTerms :: [S.TTree] -> B.Ab [S.Term [S.TTree]]
treesTerms = name where
    name [] = Right []
    name (x : xs) = do let (c, xs2) = cont xs
                       n    <- treeFlatName x
                       xs2' <- name xs2
                       Right $ (n, c) : xs2'

    cont :: [S.TTree] -> ([S.TTree], [S.TTree])
    cont xs@(x : _) | isTermLeaf x  = ([], xs)
    cont []                         = ([], [])
    cont (x : xs)                   = B.consFst x $ cont xs

    isTermLeaf (L (S.TTermN _ _ _))             = True
    isTermLeaf (L (S.TTerm _ S.TermTypePath _)) = True
    isTermLeaf _                                = False

-- | Read list of named token trees from token trees.
--   This function wraps long branches into group.
treesTerms1 :: [S.TTree] -> B.Ab [S.Term S.TTree]
treesTerms1 xs = do xs' <- treesTerms xs
                    Right $ B.mapSndTo S.ttreeGroup xs'

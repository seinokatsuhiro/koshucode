{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decode term name.

module Koshucode.Baala.Data.Decode.Term
  ( -- * Term name
    CacheT, cacheT,
    treeFlatName, treeFlatNameCached,
    treesFlatNames,
    treesTerms,
    treesTermsCached,
    treesTerms1,
    treesFlatNamePairs,
    treesNamesByColon,
    treesFlatNamesCo,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Data.Decode.Message   as Msg

import Koshucode.Baala.Syntax.TTree.Pattern


-- ----------------------  Term

-- | Term name cache.
type CacheT = O.CacheS S.TermName

-- | Empty term name cache.
cacheT :: CacheT
cacheT = O.cache [] S.stringTermName

-- | Read flat term name from token tree.
--   If the token tree contains nested term name, this function failed.
--
--   >>> S.toTree "/a" >>= treeFlatName
--   Right (TermName EQ "a")
--
--   >>> S.toTree "+/a" >>= treeFlatName
--   Right (TermName GT "a")
--
--   >>> S.toTree "/a/x" >>= treeFlatName
--   Left ...
--
treeFlatName :: S.TTree -> B.Ab S.TermName
treeFlatName = fmap snd . treeFlatNameCached cacheT

-- | Cached version of 'treeFlatName'.
treeFlatNameCached :: CacheT -> S.TTree -> B.Ab (CacheT, S.TermName)
treeFlatNameCached cc (LTerm n)   = Right $ O.cacheGet cc n
treeFlatNameCached _  (L t)       = Msg.reqFlatName t
treeFlatNameCached _  _           = Msg.reqTermName

-- | Read flat term names.
--
--   >>> S.toTrees "/a +/b -/c" >>= treesFlatNames
--   Right [TermName EQ "a",TermName GT "b",TermName LT "c"]
--
treesFlatNames :: [S.TTree] -> B.Ab [S.TermName]
treesFlatNames = mapM treeFlatName

-- | Read list of named token trees from token trees.
--
--   >>> S.toTrees "/a 'A3 /b 10" >>= treesTerms
--   Right [ (TermName EQ "a", [TreeL ...]),
--           (TermName EQ "b", [TreeL ...]) ]
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

-- | Cached version of 'treesTerms'.
treesTermsCached :: CacheT -> [S.TTree] -> B.Ab (CacheT, [S.Term [S.TTree]])
treesTermsCached = name where
    name cc [] = Right (cc, [])
    name cc (x : xs) =
        do let (c, xs2) = cont xs
           (cc1, n)    <- treeFlatNameCached cc x
           (cc2, xs2') <- name cc1 xs2
           Right (cc2, (n, c) : xs2')

    cont :: [S.TTree] -> ([S.TTree], [S.TTree])
    cont xs@(x : _) | isTermLeaf x  = ([], xs)
    cont []                         = ([], [])
    cont (x : xs)                   = B.consFst x $ cont xs

-- | Test token tree is term leaf.
isTermLeaf :: O.Test S.TTree
isTermLeaf (L (S.TTerm _ _))   = True
isTermLeaf _                   = False

-- | Read list of named token trees from token trees.
--   This function wraps long branches into group.
treesTerms1 :: [S.TTree] -> B.Ab [S.Term S.TTree]
treesTerms1 xs = do xs' <- treesTerms xs
                    Right $ B.mapSndTo S.ttreeGroup xs'

-- | Decode a list of name-and-name pairs.
-- 
--   >>> S.toTrees "/a /x /b /y" >>= treesFlatNamePairs
--   Right [(TermName EQ "a", TermName EQ "x"), (TermName EQ "b", TermName EQ "y")]
--
treesFlatNamePairs :: [S.TTree] -> B.Ab [S.TermName2]
treesFlatNamePairs = loop where
    loop (a : b : xs) = do a'  <- treeFlatName a
                           b'  <- treeFlatName b
                           xs' <- loop xs
                           Right $ (a', b') : xs'
    loop [] = Right []
    loop _  = Msg.reqTermName

-- | Decode term names grouped by colons.
--
--   >>> S.toTrees "/a /b : /p /q : /x /y" >>= treesNamesByColon
--   Right [ [TermName EQ "a", TermName EQ "b"]
--         , [TermName EQ "p", TermName EQ "q"]
--         , [TermName EQ "x", TermName EQ "y"] ]
--
treesNamesByColon :: [S.TTree] -> B.Ab [[S.TermName]]
treesNamesByColon = loop [] [] where
    loop ret ns (LTerm n  : ts)  = loop ret (S.toTermName n : ns) ts
    loop ret ns (LRaw ":" : ts)  = loop (reverse ns : ret) [] ts
    loop ret ns []               = Right $ reverse $ reverse ns : ret
    loop _ _ _                   = Msg.reqTermName

-- | Decode term names with optional complement symbol.
-- 
--   >>> S.toTrees "~ /a /b" >>= treesFlatNamesCo
--   Right (True, [TermName EQ "a",TermName EQ "b"])
-- 
--   >>> S.toTrees "/a /b" >>= treesFlatNamesCo
--   Right (False, [TermName EQ "a",TermName EQ "b"])
--
treesFlatNamesCo :: [S.TTree] -> B.Ab (Bool, [S.TermName])
treesFlatNamesCo trees =
    do (co, trees2) <- treesTermCo trees
       ns <- mapM treeFlatName trees2
       Right (co, ns)

-- | Term complement symbol.
treesTermCo :: [S.TTree] -> B.Ab (Bool, [S.TTree])
treesTermCo (LRaw "~" : trees)  = Right (True,  trees)
treesTermCo trees               = Right (False, trees)


{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decode terms.

module Koshucode.Baala.Data.Decode.Term
  ( -- * Decode terms
    -- ** Term name cache
    CacheT, cacheT,

    -- ** Term name
    treeFlatName, treeFlatNameCached,
    treesFlatNames,
    treesFlatNamesCo,
    treesFlatNamePairs,
    treesNamesByColon,

    -- ** Term
    treesTerms, treesTermsCached,
    treesTerms1,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Syntax.Pattern        as P
import qualified Koshucode.Baala.Data.Decode.Message   as Msg


-- ============================================  Decode terms

-- ---------------------------------  Term name cache

-- | Term name cache.
type CacheT t = O.Cache (Ordering, t) S.TermName

-- | Empty term name cache.
cacheT :: (S.ToTermName t) => CacheT t
cacheT = O.cache [] to where
    to (ord, n) = S.toTermNameOrd ord n

-- ---------------------------------  Term name

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
treeFlatName :: (S.ToTermName t) => S.TTree t -> B.Ab S.TermName
treeFlatName = fmap snd . treeFlatNameCached cacheT

-- | Cached version of 'treeFlatName'.
treeFlatNameCached :: (Ord t) => CacheT t -> S.TTree t -> B.Ab (CacheT t, S.TermName)
treeFlatNameCached cc (P.LTermOrd ord n) = Right $ O.cacheGet cc (ord, n)
treeFlatNameCached _  (P.L _)     = Msg.reqFlatName
treeFlatNameCached _  _           = Msg.reqTermName

-- | Read flat term names.
--
--   >>> S.toTrees "/a +/b -/c" >>= treesFlatNames
--   Right [TermName EQ "a",TermName GT "b",TermName LT "c"]
--
treesFlatNames :: (S.ToTermName t) => [S.TTree t] -> B.Ab [S.TermName]
treesFlatNames = mapM treeFlatName

-- | Decode term names with optional complement symbol.
-- 
--   >>> S.toTrees "~ /a /b" >>= treesFlatNamesCo
--   Right (True, [TermName EQ "a",TermName EQ "b"])
-- 
--   >>> S.toTrees "/a /b" >>= treesFlatNamesCo
--   Right (False, [TermName EQ "a",TermName EQ "b"])
--
treesFlatNamesCo :: (S.TextualTermName t) => [S.TTree t] -> B.Ab (Bool, [S.TermName])
treesFlatNamesCo trees =
    do (co, trees2) <- treesTermCo trees
       ns <- mapM treeFlatName trees2
       Right (co, ns)

-- | Term complement symbol.
treesTermCo :: (O.Textual t) => [S.TTree t] -> B.Ab (Bool, [S.TTree t])
treesTermCo (P.LRaw "~" : trees)  = Right (True,  trees)
treesTermCo trees                 = Right (False, trees)

-- | Decode a list of name-and-name pairs.
-- 
--   >>> S.toTrees "/a /x /b /y" >>= treesFlatNamePairs
--   Right [(TermName EQ "a", TermName EQ "x"), (TermName EQ "b", TermName EQ "y")]
--
treesFlatNamePairs :: (S.TextualTermName t) => [S.TTree t] -> B.Ab [S.TermName2]
treesFlatNamePairs = loop where
    loop (a : b : xs) = do a'  <- treeFlatName a
                           b'  <- treeFlatName b
                           xs' <- loop xs
                           Right $ (a', b') : xs'
    loop [] = Right []
    loop [_] = Msg.missPairTerm

-- | Decode term names grouped by colons.
--
--   >>> S.toTrees "/a /b : /p /q : /x /y" >>= treesNamesByColon
--   Right [ [TermName EQ "a", TermName EQ "b"]
--         , [TermName EQ "p", TermName EQ "q"]
--         , [TermName EQ "x", TermName EQ "y"] ]
--
treesNamesByColon :: (S.TextualTermName t) => [S.TTree t] -> B.Ab [[S.TermName]]
treesNamesByColon = loop [] [] where
    loop ret ns (P.LTermOrd ord n : ts) = loop ret (S.toTermNameOrd ord n : ns) ts
    loop ret ns (P.LRaw ":" : ts)  = loop (reverse ns : ret) [] ts
    loop ret ns []                 = Right $ reverse $ reverse ns : ret
    loop _ _ _                     = Msg.reqTermName


-- ---------------------------------  Terms

-- | Read list of named token trees from token trees.
--
--   >>> S.toTrees "/a 'A3 /b 10" >>= treesTerms
--   Right [ (TermName EQ "a", [TreeL ...]),
--           (TermName EQ "b", [TreeL ...]) ]
--
treesTerms :: (S.ToTermName t) => [S.TTree t] -> B.Ab [S.Term [S.TTree t]]
treesTerms = name where
    name [] = Right []
    name (x : xs) = do let (c, xs2) = cont xs
                       n    <- treeFlatName x
                       xs2' <- name xs2
                       Right $ (n, c) : xs2'

    cont xs@(x : _) | isTermLeaf x  = ([], xs)
    cont []                         = ([], [])
    cont (x : xs)                   = B.consFst x $ cont xs

-- | Test token tree is term leaf.
isTermLeaf :: O.Test (S.TTree t)
isTermLeaf (P.LTermOrd _ _)  = True
isTermLeaf _                 = False

-- | Cached version of 'treesTerms'.
treesTermsCached :: (S.ToTermName t) => CacheT t -> [S.TTree t] -> B.Ab (CacheT t, [S.Term [S.TTree t]])
treesTermsCached = name where
    name cc [] = Right (cc, [])
    name cc (x : xs) =
        do let (c, xs2) = cont xs
           (cc1, n)    <- treeFlatNameCached cc x
           (cc2, xs2') <- name cc1 xs2
           Right (cc2, (n, c) : xs2')

    cont xs@(x : _) | isTermLeaf x  = ([], xs)
    cont []                         = ([], [])
    cont (x : xs)                   = B.consFst x $ cont xs

-- | Read list of named token trees from token trees.
--   This function wraps long branches into group.
treesTerms1 :: (S.ToTermName t) => [S.TTree t] -> B.Ab [S.Term (S.TTree t)]
treesTerms1 xs = do xs' <- treesTerms xs
                    Right (S.ttreeGroup O.<$$> xs')


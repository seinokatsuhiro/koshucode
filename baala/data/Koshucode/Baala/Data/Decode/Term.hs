{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decode term name.

module Koshucode.Baala.Data.Decode.Term
  ( -- * Term name
    treeFlatName,
    treesFlatNames,
    treeSignedName,
    treesTerms,
    treesTerms1,
    treesFlatNamePairs,
    treesNamesByColon,
    treesFlatNamesCo,
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

-- | Read signed term name.
--
--   >>> S.tt1 "+/a" >>= treeSignedName
--   Right (GT, "a")
--
treeSignedName :: S.TTree -> B.Ab S.SignedTermName
treeSignedName (S.TermLeafName _ sign n) = Right (sign, n)
treeSignedName _                         = Msg.reqTermName

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

-- | Decode a list of name-and-name pairs.
-- 
--   >>> S.tt "/a /x /b /y" >>= treesFlatNamePairs
--   Right [("/a", "/x"), ("/b", "/y")]
--
treesFlatNamePairs :: [S.TTree] -> B.Ab [S.TermName2]
treesFlatNamePairs = loop where
    loop (a : b : xs) =
        do a'  <- treeFlatName a
           b'  <- treeFlatName b
           xs' <- loop xs
           Right $ (a', b') : xs'
    loop [] = Right []
    loop _  = Msg.reqTermName

-- | Decode term names grouped by colons.
--
--   >>> S.tt "/a /b : /p /q : /x /y" >>= treesNamesByColon
--   Right [["a","b"], ["p","q"], ["x","y"]]
--
treesNamesByColon :: [S.TTree] -> B.Ab [[S.TermName]]
treesNamesByColon = loop [] [] where
    loop ret ns (S.TermLeafName _ _ n : ts) = loop ret (n : ns) ts
    loop ret ns (S.TermLeaf _ _ [n] : ts)   = loop ret (n : ns) ts
    loop ret ns (S.TextLeafRaw _ ":" : ts)  = loop (reverse ns : ret) [] ts
    loop ret ns []                          = Right $ reverse $ reverse ns : ret
    loop _ _ _                              = Msg.reqTermName

-- | Decode term names with optional complement symbol.
-- 
--   >>> S.tt "~ /a /b" >>= treesFlatNamesCo
--   Right (True, ["a","b"])
-- 
--   >>> S.tt "/a /b" >>= treesFlatNamesCo
--   Right (False, ["a","b"])
--
treesFlatNamesCo :: [S.TTree] -> B.Ab (Bool, [S.TermName])
treesFlatNamesCo trees =
    do (co, trees2) <- treesTermCo trees
       ns <- mapM treeFlatName trees2
       Right (co, ns)

-- Term complement symbol
treesTermCo :: [S.TTree] -> B.Ab (Bool, [S.TTree])
treesTermCo (S.TextLeafRaw _ "~" : trees)  = Right (True,  trees)
treesTermCo trees                          = Right (False, trees)


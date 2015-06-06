{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parsing list of terms.

module Koshucode.Baala.Op.Builtin.Term
  ( termName, termNames, termNamesCo,
    termNamePairs, termNamesColon,
    picker,
  ) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Op.Message as Msg

-- | Extract a term name.
termName :: B.TTree -> B.Ab B.TermName
termName (B.TermLeaf _ _ [n]) = Right n
termName _ = Msg.reqTermName

-- | Extract a list of term names.
-- 
--   >>> termNames B.<=< B.tt $ "/a /b /c"
--   Right ["a", "b", "c"]
termNames :: [B.TTree] -> B.Ab [B.TermName]
termNames = mapM termName

termNamesCo :: [B.TTree] -> B.Ab (Bool, [B.TermName])
termNamesCo trees =
    do (co, trees2) <- termCo trees
       ns <- mapM termName trees2
       Right (co, ns)

-- Term complement symbol
termCo :: [B.TTree] -> B.Ab (Bool, [B.TTree])
termCo (B.TextLeafRaw _ "~" : trees)  = Right (True,  trees)
termCo trees                          = Right (False, trees)

-- | Extract a list of name-and-name pairs.
-- 
--   >>> termNamePairs =<< B.tt "/a /x /b /y"
--   Right [("/a", "/x"), ("/b", "/y")]
termNamePairs :: [B.TTree] -> B.Ab [B.TermName2]
termNamePairs = loop where
    loop (a : b : xs) =
        do a'  <- termName a
           b'  <- termName b
           xs' <- loop xs
           Right $ (a', b') : xs'
    loop [] = Right []
    loop _  = Msg.reqTermName

-- >>> termNamesColon =<< B.tt "/a /b : /x /y"
-- Right (["a", "b"], ["x", "y"])
termNamesColon :: [B.TTree] -> B.Ab ([B.TermName], [B.TermName])
termNamesColon = first [] where
    first ns1 (B.TermLeaf _ _ [n] : ts)       = first (n : ns1) ts
    first ns1 (B.TextLeafRaw _ ":" : ts)      = second ns1 [] ts
    first _ _                                 = Msg.reqTermName

    second ns1 ns2 (B.TermLeaf _ _ [n] : ts)  = second ns1 (n : ns2) ts
    second ns1 ns2 []                         = Right (reverse ns1, reverse ns2)
    second _ _ _                              = Msg.reqTermName

picker :: B.Head -> [B.TermName] -> B.Map [c]
picker he ts = B.snipFrom ind where
    ind = ts `B.snipIndex` B.headNames he


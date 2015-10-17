{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parsing list of terms.

module Koshucode.Baala.Op.Builtin.Term
  ( termName, termNames, termNamesCo,
    termNamePairs, termNamesColon,
    picker,
  ) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Data       as D
import qualified Koshucode.Baala.Op.Message as Msg

-- | Extract a term name.
termName :: D.TTree -> B.Ab D.TermName
termName (D.TermLeafName _ n) = Right n
termName (D.TermLeaf _ _ [n]) = Right n
termName _                    = Msg.reqTermName

-- | Extract a list of term names.
-- 
--   >>> termNames B.<=< B.tt $ "/a /b /c"
--   Right ["a", "b", "c"]
termNames :: [D.TTree] -> B.Ab [D.TermName]
termNames = mapM termName

termNamesCo :: [D.TTree] -> B.Ab (Bool, [D.TermName])
termNamesCo trees =
    do (co, trees2) <- termCo trees
       ns <- mapM termName trees2
       Right (co, ns)

-- Term complement symbol
termCo :: [D.TTree] -> B.Ab (Bool, [D.TTree])
termCo (D.TextLeafRaw _ "~" : trees)  = Right (True,  trees)
termCo trees                          = Right (False, trees)

-- | Extract a list of name-and-name pairs.
-- 
--   >>> termNamePairs =<< B.tt "/a /x /b /y"
--   Right [("/a", "/x"), ("/b", "/y")]
termNamePairs :: [D.TTree] -> B.Ab [D.TermName2]
termNamePairs = loop where
    loop (a : b : xs) =
        do a'  <- termName a
           b'  <- termName b
           xs' <- loop xs
           Right $ (a', b') : xs'
    loop [] = Right []
    loop _  = Msg.reqTermName

-- >>> termNamesColon =<< B.tt "/a /b : /x /y"
-- Right [["a", "b"], ["x", "y"]]
termNamesColon :: [D.TTree] -> B.Ab [[D.TermName]]
termNamesColon = loop [] [] where
    loop ret ns (D.TermLeafName _ n : ts)   = loop ret (n : ns) ts
    loop ret ns (D.TermLeaf _ _ [n] : ts)   = loop ret (n : ns) ts
    loop ret ns (D.TextLeafRaw _ ":" : ts)  = loop (reverse ns : ret) [] ts
    loop ret ns []                          = Right $ reverse $ reverse ns : ret
    loop _ _ _                              = Msg.reqTermName

picker :: D.Head -> [D.TermName] -> B.Map [c]
picker he ts = B.snipFrom ind where
    ind = ts `B.snipIndex` D.headNames he


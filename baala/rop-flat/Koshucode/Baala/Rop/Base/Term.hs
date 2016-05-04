{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parsing list of terms.

module Koshucode.Baala.Rop.Base.Term
  ( termName, termNames, termNamesCo,
    signedTermName, signedTermNames,
    termNamePairs, termNamesColon,
    picker,
  ) where

import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Syntax           as S
import qualified Koshucode.Baala.Data             as D
import qualified Koshucode.Baala.Rop.Base.Message as Msg

-- | Extract a term name.
termName :: S.TTree -> B.Ab S.TermName
termName (S.TermLeafName _ _ n) = Right n
termName (S.TermLeaf _ _ [n])   = Right n
termName _                      = Msg.reqTermName

-- | Extract a list of term names.
-- 
--   >>> termNames B.<=< D.tt $ "/a /b /c"
--   Right ["a", "b", "c"]
termNames :: [S.TTree] -> B.Ab [S.TermName]
termNames = mapM termName

-- | Extract a signed term name.
signedTermName :: S.TTree -> B.Ab S.SignedTermName
signedTermName (S.TermLeafName _ sign n) = Right (sign, n)
signedTermName _                         = Msg.reqTermName

signedTermNames :: [S.TTree] -> B.Ab [S.SignedTermName]
signedTermNames = mapM signedTermName

termNamesCo :: [S.TTree] -> B.Ab (Bool, [S.TermName])
termNamesCo trees =
    do (co, trees2) <- termCo trees
       ns <- mapM termName trees2
       Right (co, ns)

-- Term complement symbol
termCo :: [S.TTree] -> B.Ab (Bool, [S.TTree])
termCo (S.TextLeafRaw _ "~" : trees)  = Right (True,  trees)
termCo trees                          = Right (False, trees)

-- | Extract a list of name-and-name pairs.
-- 
--   >>> termNamePairs =<< D.tt "/a /x /b /y"
--   Right [("/a", "/x"), ("/b", "/y")]
termNamePairs :: [S.TTree] -> B.Ab [S.TermName2]
termNamePairs = loop where
    loop (a : b : xs) =
        do a'  <- termName a
           b'  <- termName b
           xs' <- loop xs
           Right $ (a', b') : xs'
    loop [] = Right []
    loop _  = Msg.reqTermName

-- >>> termNamesColon =<< D.tt "/a /b : /x /y"
-- Right [["a", "b"], ["x", "y"]]
termNamesColon :: [S.TTree] -> B.Ab [[S.TermName]]
termNamesColon = loop [] [] where
    loop ret ns (S.TermLeafName _ _ n : ts) = loop ret (n : ns) ts
    loop ret ns (S.TermLeaf _ _ [n] : ts)   = loop ret (n : ns) ts
    loop ret ns (S.TextLeafRaw _ ":" : ts)  = loop (reverse ns : ret) [] ts
    loop ret ns []                          = Right $ reverse $ reverse ns : ret
    loop _ _ _                              = Msg.reqTermName

picker :: D.Head -> [S.TermName] -> B.Map [c]
picker he ts = B.snipFrom ind where
    ind = ts `B.snipIndex` D.headNames he


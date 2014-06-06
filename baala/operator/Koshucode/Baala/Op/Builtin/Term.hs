{-# OPTIONS_GHC -Wall #-}

-- | Parsing list of terms.

module Koshucode.Baala.Op.Builtin.Term
( termName, termNames, termNamesCo,
  termNamePairs, termTreePairs,
) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Core.Message as Message

-- | Extract a term name.
termName :: B.TokenTree -> B.Ab B.TermName
termName (B.TreeL (B.TTerm _ [n])) = Right n
termName _ = Message.reqTermName

-- | Extract a list of term names.
-- 
--   >>> termNames B.<=< B.tt $ "/a /b /c"
--   Right ["a", "b", "c"]
termNames :: [B.TokenTree] -> B.Ab [B.TermName]
termNames = mapM termName

termNamesCo :: [B.TokenTree] -> B.Ab (Bool, [B.TermName])
termNamesCo trees =
    do (co, trees2) <- termCo trees
       ns <- mapM termName trees2
       Right (co, ns)

-- Term complement symbol
termCo :: [B.TokenTree] -> B.Ab (Bool, [B.TokenTree])
termCo (B.TreeL (B.TText _ 0 "~") : trees) = Right (True, trees)
termCo trees                               = Right (False, trees)

-- | Extract a list of name-and-name pairs.
-- 
--   >>> termNamePairs . B.tt $ "/a /x /b /y"
--   Right [("/a", "/x"), ("/b", "/y")]
termNamePairs :: [B.TokenTree] -> B.Ab [B.TermName2]
termNamePairs = loop where
    loop (a : b : xs) =
        do a'  <- termName a
           b'  <- termName b
           xs' <- loop xs
           Right $ (a', b') : xs'
    loop [] = Right []
    loop _  = Message.reqTermName

-- | Extract a list of name-and-tree pairs.
-- 
--   >>> termTreePairs B.<=< B.tt $ "/a 'A3 /b 10 /c"
--   Right [ ("a", TreeL (TText 3 1 "A3"))
--         , ("b", TreeL (TText 7 0 "10"))
--         , ("c", TreeB 1 []) ]
termTreePairs :: [B.TokenTree] -> B.Ab [B.Named B.TokenTree]
termTreePairs = C.litNamedTrees


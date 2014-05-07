{-# OPTIONS_GHC -Wall #-}

-- | Parsing list of terms.

module Koshucode.Baala.Op.Builtin.Term
( termName,
  termNames,
  termNamePairs,
  withTerms,
  termTreePairs,
) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Core.Message as Message



-- ----------------------  Term

-- | Extract a term name.
termName :: B.TokenTree -> B.Ab B.TermName
termName (B.TreeL (B.TTerm _ [n])) = Right n
termName _ = Message.reqTermName

-- | Extract a list of term names.
-- 
--   >>> termNames . B.tt $ "/a /b /c"
--   Right ["/a", "/b", "/c"]
termNames :: [B.TokenTree] -> B.Ab [B.TermName]
termNames trees =
    case mapM termName trees of
      Right ns -> Right ns
      Left  _  -> Message.reqTermName

-- | Extract a list of name-and-name pairs.
-- 
--   >>> termNamePairs . B.tt $ "/a /x /b /y"
--   Right [("/a", "/x"), ("/b", "/y")]
termNamePairs :: [B.TokenTree] -> B.Ab [(B.TermName, B.TermName)]
termNamePairs = loop where
    loop (a : b : xs) =
        do a'  <- termName a
           b'  <- termName b
           xs' <- loop xs
           Right $ (a', b') : xs'
    loop [] = Right []
    loop _  = Message.reqTermName

withTerms :: [B.TokenTree] -> B.Ab [B.Terminal String]
withTerms = loop where
    loop (B.TreeL (B.TTerm _ [n]) : B.TreeL (B.TWord _ 0 v) : xs) = next (n, v) xs
    loop (B.TreeL (B.TTerm _ [n]) : xs)                           = next (n, n) xs
    loop [] = Right []
    loop _  = Message.reqTermName

    next p xs = do xs' <- loop xs
                   Right $ p : xs'

-- | Extract a list of name-and-tree pairs.
-- 
--   >>> termTreePairs . B.tt $ "/a 'A3 /b 10 /c"
--   Right [ ("/a", TreeL (TWord 3 1 "A3"))
--         , ("/b", TreeL (TWord 7 0 "10"))
--         , ("/c", TreeB 1 []) ]
termTreePairs :: [B.TokenTree] -> B.Ab [B.Named B.TokenTree]
termTreePairs xs = C.litNamedTrees xs


{-# OPTIONS_GHC -Wall #-}

-- | Utilities for token trees.

module Koshucode.Baala.Core.Content.Tree
  ( treesToTokens,
    treesToTexts,
    treeToText,
    treesToDigits,
    treeToFlatTerm,
    treesToTerms,
    treesToTerms1,
    treesToInterp,
    -- $Function
  ) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core.Message  as Msg

-- $Function
--
--  /Example/
--
--  >>> treesToTerms =<< B.tt "/a 'A3 /b 10"
--  Right [("/a", [TreeL ...]),
--         ("/b", [TreeL ...])]
--

-- ----------------------  Token

treesToTokens :: B.TTreesToAb [B.Token]
treesToTokens = mapM token where
    token (B.TreeL t) = Right t
    token _ = Msg.adlib "not token"

-- ----------------------  Text

treesToTexts :: Bool -> B.TTreesToAb [String]
treesToTexts q = loop [] where
    loop ss [] = Right $ reverse ss
    loop ss (B.TreeL x : xs) = do s <- tokenToText q x
                                  loop (s : ss) xs
    loop _ _ = Msg.nothing

treeToText :: Bool -> B.TTreeToAb String
treeToText q (B.TreeL tok) = tokenToText q tok
treeToText _ _ = Msg.nothing

-- | Get quoted/unquoted text.
tokenToText :: Bool -> B.Token -> B.Ab String
tokenToText True  (B.TText _ q w) | q > 0   =  Right w
tokenToText False (B.TText _ q w) | q == 0  =  Right w
tokenToText _ _  =  Msg.nothing

treesToDigits :: B.TTreesToAb String
treesToDigits = concatDigits B.<=< treesToTexts False where

concatDigits :: [String] -> B.Ab String
concatDigits = first where
    first ((c : cs) : xs) | c `elem` "+-0123456789" = loop [[c]] $ cs : xs
    first _ = Msg.nothing

    loop ss [] = Right $ concat $ reverse ss
    loop ss (w : xs) | all (`elem` "0123456789.") w = loop (w:ss) xs
    loop _ _ = Msg.nothing

-- ----------------------  Term

-- | Get flat term name from token tree.
--   If the token tree contains nested term name, this function failed.
treeToFlatTerm :: B.TTreeToAb B.TermName
treeToFlatTerm (B.TreeL (B.TTerm _ 0 [n])) = Right n
treeToFlatTerm (B.TreeL t)                 = Msg.reqFlatName t
treeToFlatTerm _                           = Msg.reqTermName

-- | Convert token trees into a list of named token trees.
treesToTerms :: B.TTreesToAb [B.NamedTrees]
treesToTerms = name where
    name [] = Right []
    name (x : xs) = do let (c, xs2) = cont xs
                       n    <- treeToFlatTerm x
                       xs2' <- name xs2
                       Right $ (n, c) : xs2'

    cont :: B.TTreesTo ([B.TTree], [B.TTree])
    cont xs@(B.TreeL (B.TTerm _ 0 _) : _) = ([], xs)
    cont [] = ([], [])
    cont (x : xs) = B.cons1 x $ cont xs

treesToTerms1 :: B.TTreesToAb [B.NamedTree]
treesToTerms1 xs = do xs' <- treesToTerms xs
                      Right $ B.mapSndTo B.wrapTrees xs'

-- ----------------------  Interp

treesToInterp :: B.TTreesToAb B.Interp
treesToInterp = Right . B.interp B.<=< mapM treeToInterpWord

treeToInterpWord :: B.TTreeToAb B.InterpWord
treeToInterpWord (B.TreeB _ _ _) = Msg.nothing
treeToInterpWord (B.TreeL x) =
    case x of
      B.TText _ _ w    ->  Right $ B.InterpText w
      B.TTerm _ _ [n]  ->  Right $ B.InterpTerm n
      _                ->  Msg.nothing
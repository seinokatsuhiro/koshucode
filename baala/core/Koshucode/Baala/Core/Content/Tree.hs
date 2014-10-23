{-# OPTIONS_GHC -Wall #-}

-- | Utilities for token trees.

module Koshucode.Baala.Core.Content.Tree
  ( treesToTokens,
    treesToTexts,
    treeToText,
    treesToDigits,
    TimeText,
    treesToTime,
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
treesToDigits = concatDigits B.<=< treesToTexts False

concatDigits :: [String] -> B.Ab String
concatDigits = first where
    first ((c : cs) : xs) | c `elem` "+-0123456789" = loop [[c]] $ cs : xs
    first _ = Msg.nothing

    loop ss [] = Right $ concat $ reverse ss
    loop ss (w : xs) | all (`elem` "0123456789.") w = loop (w:ss) xs
    loop _ _ = Msg.nothing


-- ----------------------  Time

type TimeText = ((String, String, Maybe String), (Maybe String, Maybe String, Maybe String))

treesToTime :: B.TTreesToAb TimeText
treesToTime = concatTime B.<=< treesToTexts False

concatTime :: [String] -> B.Ab TimeText
concatTime = year where
    year ((a:b:c:d: '-' :x) : xs) | isDigit4 a b c d = month [a,b,c,d] (x : xs)
    year _  = Msg.nothing

    month y ((a:b: '-' :x) : xs) | isDigit2 a b = day y [a,b] (x : xs)
    month y [m@[a, b]] | isDigit2 a b = Right ((y, m, Nothing), (Nothing, Nothing, Nothing))
    month _ _ = Msg.nothing

    day y m (d@[a,b] : xs) | isDigit2 a b = hour (y, m, Just d) xs
    day _ _ _ = Msg.nothing

    hour ymd xs     = do (h, xs') <- ddc xs
                         minute ymd h xs'
    minute ymd h xs = do (i, xs') <- ddc xs
                         second ymd h i xs'

    second ymd h i (s@[a,b] : _) | isDigit2 a b = Right (ymd, (h, i, Just s))
    second ymd h i [] = Right (ymd, (h, i, Nothing))
    second _ _ _ _ = Msg.nothing

    ddc :: [String] -> B.Ab (Maybe String, [String])
    ddc (x@[a,b] : ":" : xs) | isDigit2 a b  =  Right (Just x, xs)
    ddc (x@[a,b] : [])       | isDigit2 a b  =  Right (Just x, [])
    ddc []                                   =  Right (Nothing, [])
    ddc _                                    =  Msg.nothing

isDigit :: Char -> Bool
isDigit = (`elem` "0123456789")

isDigit2 :: Char -> Char -> Bool
isDigit2 a b = isDigit a && isDigit b

isDigit4 :: Char -> Char -> Char -> Char -> Bool
isDigit4 a b c d = isDigit2 a b && isDigit2 c d


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

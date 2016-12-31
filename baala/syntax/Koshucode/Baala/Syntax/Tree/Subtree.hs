{-# OPTIONS_GHC -Wall #-}

-- | Subtree clause.

module Koshucode.Baala.Syntax.Tree.Subtree
  ( readSubtreeClauses,
    SubtreeFilter (..),
    subtreeId, subtreeEq,
    subtreeKeep, subtreeOmit,
    subtreeChain,
    subtreeFilter,
  ) where

import qualified System.FilePath.Glob                    as Glob
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.Token.Pattern    as P

-- | Read subtree clauses from file.
--
--   >>> mapM_ (S.printToks . B.clauseTokens) =<< B.abortLeft =<< readSubtreeClauses "subtree.txt"
--   ********** > "a1"
--   TText /0.10.0/ TextRaw ">"
--   TText /0.10.2/ TextQQ "a1"
--   TOpen /0.0.0/ "("
--   TText /0.11.2/ TextRaw "-"
--   TText /0.11.4/ TextQQ "bb"
--   TText /0.0.0/ TextRaw "|"
--   TText /0.12.2/ TextRaw "-"
--   TText /0.12.4/ TextQQ "cc"
--   TClose /0.0.0/ ")"
--
readSubtreeClauses :: FilePath -> B.IOAb [S.TokenClause]
readSubtreeClauses path =
    do ls' <- S.readClauses path
       return $ case ls' of
         Left a -> Left a
         Right ls -> subtreeClause O.<#> ls

subtreeClause :: S.TokenClause -> B.Ab S.TokenClause
subtreeClause cl = cl' where
    cl' = case clauseFirstElem cl of
            Just (P.TRaw s) | s `elem` [">", ">>"] ->
                do toks <- branch (B.lineTokens <$> B.clauseLines cl)
                   Right $ cl { B.clauseTokens = S.sweepToken $ concat toks }
            _ -> Right cl

    branch = B.indentBranch size test open sep close

    size (S.TSpace _ n) = Just n
    size _              = Nothing

    test (P.TRaw s) = s `elem` [">", ">>", "-", "+"]
    test _          = False

    open   = S.TOpen  B.def "("
    sep    = S.TText  B.def S.TextRaw "|"
    close  = S.TClose B.def ")"

clauseFirstElem :: B.CodeClause a -> Maybe a
clauseFirstElem cl =
    case B.clauseLines cl of
      [] -> Nothing
      ln : _ -> case B.lineTokens ln of
                  [] -> Nothing
                  tok : _ -> Just tok

-- | Subtree filter.
data SubtreeFilter
    = SubtreeId                                 -- ^ Identity
    | SubtreeEq    String                       -- ^ Equality
    | SubtreeKeep  String (Maybe Glob.Pattern)  -- ^ Glob
    | SubtreeOmit  String (Maybe Glob.Pattern)  -- ^ Anti-glob
    | SubtreeChain SubtreeFilter SubtreeFilter  -- ^ Filter chain
      deriving (Show, Eq)

instance Monoid SubtreeFilter where
    mempty  = SubtreeId
    mappend = SubtreeChain

-- | Identity filter.
subtreeId :: SubtreeFilter
subtreeId = SubtreeId

-- | Equal filter.
subtreeEq :: String -> SubtreeFilter
subtreeEq = SubtreeEq

-- | Glob filter.
subtreeKeep :: String -> SubtreeFilter
subtreeKeep s = SubtreeKeep s Nothing

-- | Anti-glob filter.
subtreeOmit :: String -> SubtreeFilter
subtreeOmit s = SubtreeOmit s Nothing

-- | Filter chain.
subtreeChain :: O.Bin SubtreeFilter
subtreeChain = SubtreeChain

-- | Filter strings by subtree filter.
--
--   >>> subtreeFilter subtreeId ["foo", "bar", "foobar"]
--   ["foo","bar","foobar"]
--
--   >>> subtreeFilter (SubtreeEq "bar") ["foo", "bar", "foobar"]
--   ["bar"]
--
--   >>> subtreeFilter (subtreeKeep "foo*") ["foo", "bar", "foobar"]
--   ["foo","foobar"]
--
--   >>> subtreeFilter (subtreeKeep "*bar") ["foo", "bar", "foobar"]
--   ["bar","foobar"]
--
--   >>> subtreeFilter (subtreeOmit "foo*") ["foo", "bar", "foobar"]
--   ["bar"]
--
--   >>> subtreeFilter (subtreeOmit "foo*") ["foo", "bar", "foobar"]
--   ["bar"]
--
--   >>> subtreeFilter (subtreeKeep "foo*" O.++ subtreeOmit "*bar") ["foo", "bar", "foobar"]
--   ["foo"]
--
subtreeFilter :: SubtreeFilter -> O.Map [String]
subtreeFilter f xs0 = loop xs0 [f] where
    loop xs []                       = xs
    loop xs (SubtreeId : fs)         = loop xs fs
    loop xs (SubtreeEq x : fs)       = loop (filter (== x) xs) fs

    loop xs (SubtreeKeep _ (Just p) : fs)  = loop (Glob.match p `filter` xs) fs
    loop xs (SubtreeKeep x (Nothing) : fs) = let p = Just $ Glob.compile x
                                              in loop xs (SubtreeKeep x p : fs)

    loop xs (SubtreeOmit _ (Just p) : fs)  = loop (Glob.match p `B.omit` xs) fs
    loop xs (SubtreeOmit x (Nothing) : fs) = let p = Just $ Glob.compile x
                                              in loop xs (SubtreeOmit x p : fs)

    loop xs (SubtreeChain f1 f2 : fs)      = loop xs $ f1 : f2 : fs


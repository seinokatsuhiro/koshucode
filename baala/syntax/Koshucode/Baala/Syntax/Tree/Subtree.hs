{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Subtree clause.

module Koshucode.Baala.Syntax.Tree.Subtree
  ( readSubtreeClauses,

    Subtree,
    SubtreePattern (..),
    subtree,

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

-- | Subtree.
type Subtree = B.RawTree [SubtreePattern] String String

-- | Subtree pattern.
data SubtreePattern
    = SubtreeL SubtreeFilter                   -- ^ Leaf
    | SubtreeB SubtreeFilter [SubtreePattern]  -- ^ Branch
    | SubtreeR SubtreeFilter [SubtreePattern]  -- ^ Recursive branch
      deriving (Show, Eq)

-- | Select subtree.
--
--   >>> let tree = B.TreeB [] "Y1" [B.TreeL "Z1", B.TreeL "Z2", B.TreeB [] "Y2" [B.TreeL "Z3"]]
--   >>> O.putLines $ B.ppTree tree
--   > [] "Y1"
--     - "Z1"
--     - "Z2"
--     > [] "Y2"
--       - "Z3"
--
--   >>> O.putLines (B.ppTree O.<++> subtree [SubtreeB (subtreeEq "Y1") [SubtreeL (subtreeEq "Z1")]] [tree])
--   > [] "Y1"
--     - "Z1"
--
--   >>> O.putLines (B.ppTree O.<++> subtree [SubtreeR (subtreeId) [SubtreeL (subtreeEq "Z3")]] [tree])
--   > [] "Y1"
--     > [] "Y2"
--       - "Z3"
--
--   >>> O.putLines (B.ppTree O.<++> subtree [SubtreeR (subtreeId) [SubtreeL (subtreeEq "Z1")]] [tree])
--   > [] "Y1"
--     - "Z1"
--
subtree :: [SubtreePattern] -> O.Map [Subtree]
subtree ps ts = subtreeRec $ subtreeOne ps ts

subtreeRec :: O.Map [Subtree]
subtreeRec ts = p O.<?> ts where
    p t@(B.TreeL _) = Just t
    p (B.TreeB ps y zs) =
        case subtreeRec $ subtreeOne ps zs of
          []  -> Nothing
          zs' -> Just $ B.TreeB [] y zs'

subtreeOne :: [SubtreePattern] -> O.Map [Subtree]
subtreeOne ps0 ts = p1 O.<?> ts where
    p1 t = maybeHead (p2 t O.<?> ps0)

    p2 t@(B.TreeL _) (SubtreeL f)
        | nullZ f t  = Nothing
        | otherwise  = Just t
    p2 t@(B.TreeB _ y zs) (SubtreeB f ps)
        | nullY f t  = Nothing
        | otherwise  = Just $ B.TreeB ps y zs
    p2 t@(B.TreeB _ y zs) (SubtreeR f ps)
        | nullY f t  = Nothing
        | otherwise  = Just $ B.TreeB (SubtreeR f ps : ps) y zs
    p2 _ _ = Nothing

    nullZ f t = null $ subtreeFilterOn getTreeZ f [t]
    nullY f t = null $ subtreeFilterOn getTreeY f [t]

maybeHead :: [a] -> Maybe a
maybeHead []       = Nothing
maybeHead (a : _)  = Just a

-- | Branch element of tree.
getTreeY :: B.RawTree b y z -> Maybe y
getTreeY (B.TreeB _ y _)  = Just y
getTreeY _                = Nothing

-- | Leaf element of tree.
getTreeZ :: B.RawTree b y z -> Maybe z
getTreeZ (B.TreeL z)  = Just z
getTreeZ _            = Nothing

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
subtreeFilter = subtreeFilterOn Just

subtreeFilterOn :: (a -> Maybe String) -> SubtreeFilter -> O.Map [a]
subtreeFilterOn get f xs0 = loop xs0 [f] where
    loop xs []                       = xs
    loop xs (SubtreeId : fs)         = loop xs fs
    loop xs (SubtreeEq x : fs)       = loop (keepOn get (== x) xs) fs

    loop xs (SubtreeKeep _ (Just p) : fs)  = loop (keepOn get (Glob.match p) xs) fs
    loop xs (SubtreeKeep x (Nothing) : fs) = let p = Just $ Glob.compile x
                                              in loop xs (SubtreeKeep x p : fs)

    loop xs (SubtreeOmit _ (Just p) : fs)  = loop (omitOn get (Glob.match p) xs) fs
    loop xs (SubtreeOmit x (Nothing) : fs) = let p = Just $ Glob.compile x
                                              in loop xs (SubtreeOmit x p : fs)

    loop xs (SubtreeChain f1 f2 : fs)      = loop xs $ f1 : f2 : fs

keepOn :: (a -> Maybe b) -> O.Test b -> O.Map [a]
keepOn get f = loop where
    loop (x@(get -> Just x') : xs)
        | f x'       = x : loop xs
        | otherwise  = loop xs
    loop (_ : xs)    = loop xs
    loop []          = []

omitOn :: (a -> Maybe b) -> O.Test b -> O.Map [a]
omitOn get f = keepOn get (not . f)


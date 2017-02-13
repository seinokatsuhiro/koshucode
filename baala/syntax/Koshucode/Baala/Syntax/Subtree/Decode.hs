{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decode subtree.

module Koshucode.Baala.Syntax.Subtree.Decode
  ( -- * Decode
    readSubtreeClauses,
    subtreeClause,
    decodeSubtreePattern,

    -- * Syntax of subtree
    -- $Syntax
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Subtext                 as U
import qualified Koshucode.Baala.Syntax.Symbol           as S
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.Tree             as S
import qualified Koshucode.Baala.Syntax.Subtree.Subtree  as S
import qualified Koshucode.Baala.Syntax.Token.Pattern    as P
import qualified Koshucode.Baala.Syntax.Tree.Pattern     as P
import qualified Koshucode.Baala.Base.Message            as Msg

-- | Read subtree clauses from file.
--
--   >>> mapM_ (S.printToks . B.clauseTokens) O.# B.abortLeft O.# readSubtreeClauses "subtree.txt"
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
readSubtreeClauses :: (S.TextualTermName t) => FilePath -> B.IOAb [S.TokenClause t]
readSubtreeClauses path =
    do ls' <- S.readClauses path
       return $ case ls' of
         Left a -> Left a
         Right ls -> subtreeClause O.<#> ls

-- | Insert open\/sep\/close to subtree clause.
subtreeClause :: (O.Textual t) => S.TokenClause t -> B.Ab (S.TokenClause t)
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
    sep    = S.TText  B.def S.TextRaw "||"
    close  = S.TClose B.def ")"

clauseFirstElem :: B.CodeClause k t -> Maybe (k t)
clauseFirstElem cl =
    case B.clauseLines cl of
      [] -> Nothing
      ln : _ -> case B.lineTokens ln of
                  [] -> Nothing
                  tok : _ -> Just tok

-- | Decode token trees to subtree patterns.
--
--   >>> S.withTrees decodeSubtreePattern "- \"Z1\""
--   Right [SubtreeL SubtreeNone (SivmapEq "Z1")]
--
--   >>> S.withTrees decodeSubtreePattern "> \"Y1\" ( - \"Z1\" )"
--   Right [SubtreeB SubtreeNone (SivmapEq "Y1") [SubtreeL SubtreeNone (SivmapEq "Z1")]]
--
--   >>> S.withTrees decodeSubtreePattern "> \"Y1\" ( - \"Z1\" || - \"Z2\" )"
--   Right [SubtreeB SubtreeNone (SivmapEq "Y1")
--            [SubtreeL SubtreeNone (SivmapEq "Z1"),
--             SubtreeL SubtreeNone (SivmapEq "Z2")]]
--
--   >>> S.withTrees decodeSubtreePattern ">> \"Y1\" ( - \"Z1\" )"
--   Right [SubtreeR (SivmapEq "Y1") [SubtreeL SubtreeNone (SivmapEq "Z1")]]
--
--   >>> S.withTrees decodeSubtreePattern ">> \"Y1\" ( - A B /z \"Z1\" )"
--   Right [SubtreeR (SivmapEq "Y1") [SubtreeL (SubtreeText ["A","B"] (TermName EQ "z")) (SivmapEq "Z1")]]
--
decodeSubtreePattern :: (S.TextualTermName t, U.ToSivExpr t)
   => [S.TTree t] -> B.Ab [S.SubtreePattern]
decodeSubtreePattern = pats where
    pats ts = pat O.<#> S.divideTreesByBar2 ts

    pat (P.LRaw "-" : ts) = do (ts', term) <- splitClassTerm ts
                               (f, _) <- filtPat ts'
                               Right $ S.SubtreeL term (O.tString <$> f)
    pat (P.LRaw ">" : ts) = do (ts', term) <- splitClassTerm ts
                               S.SubtreeB term </> ts'
    pat (P.LRaw ">>" : ts) = S.SubtreeR </> ts
    pat ts = abSubtree ts $ Msg.adlib "Unknown subtree pattern"

    k </> ts = do (f, ps) <- filtPat ts
                  Right $ k (O.tString <$> f) ps

    filtPat ts = abSubtree ts $ case splitFilter ts of
        (fs, [P.BGroup ts2])
                 -> do fs' <- treesSivmap O.<#> fs
                       ps <- pats ts2
                       Right (mconcat fs', ps)
        (fs, []) -> do fs' <- treesSivmap O.<#> fs
                       Right (mconcat fs', [])
        (_, ts2) -> abSubtree ts2 $ Msg.adlib "Malformed subtree pattern"

treesSivmap :: (O.Textual t, U.ToSivExpr t) => [S.TTree t] -> B.Ab (U.Sivmap t)
treesSivmap []                      = Right $ U.sivmapId
treesSivmap [P.LQq s]               = Right $ U.sivmapEq s
treesSivmap [P.LRaw "?", P.LQq s]   = Right $ U.sivmapKeep s
treesSivmap [P.LRaw "!", P.LQq s]   = Right $ U.sivmapOmit s
treesSivmap [P.LQq n, P.LRaw "=", P.LQq v]  = Right $ U.sivmapAssoc n v
treesSivmap ts = abSubtree ts $ Msg.adlib "Unknown subtree filter"

splitFilter :: (O.Textual t) => [S.TTree t] -> ([[S.TTree t]], [S.TTree t])
splitFilter ts = (filt', pat) where
    (filt, pat) = break isGroup ts
    filt' = S.divideTreesByBar filt

    isGroup (P.BGroup _) = True
    isGroup _            = False

splitClassTerm :: (S.TextualTermName t) => [S.TTree t] -> B.Ab ([S.TTree t], S.SubtreeTerm)
splitClassTerm ts0 = abSubtree ts0 $ loop [] ts0 where
    loop cs (P.LRaw c : ts)  = loop (c : cs) ts
    loop cs (P.LTermOrd ord n : P.LRaw "seq" : ts)
        = right cs (ts, S.SubtreeSeq (O.tString <$> reverse cs) (S.toTermNameOrd ord n))
    loop cs (P.LTermOrd ord n : ts)
        = right cs (ts, S.subtreeTexts (O.tString <$> reverse cs) (S.toTermNameOrd ord n))
    loop _ _ = Right (ts0, S.SubtreeNone)

    right cs result
        | null cs    = Msg.adlib "Expect one or more judgement classes"
        | otherwise  = Right result

-- | Abortable scope for subtree.
abSubtree :: (B.GetCodePos cp) => B.Abortable cp b
abSubtree = B.abortable "subtree"

-- $Syntax
--
-- == Pattern
--
--  [@-@ /Filter/]
--    Leaf which satisfies /Filter/.
--  [@>@ /Filter/ @{@ /Pattern/ @|@ ... @}@]
--    Branch which satisfies /Filter/,
--    and inner trees match /Pattern/.
--  [@>>@ /Filter/ @{@ /Pattern/ @|@ ... @}@]
--    Recursive branch pattern include current branch,
--    and inner trees match /Pattern/.
--  [@>>>@ /Filter/ @{@ /Pattern/ @|@ ... @}@]
--    Recursive branch pattern only inner branches,
--    and inner trees match /Pattern/.
--
-- == Filter
--
--  [/Element/ @|@ ...]
--    Chain of filter /Element/s.
--
-- == Element
--
--  [@"@/Text/@"@]
--    Node name is equal to /Text/.
--  [@?@ @"@/Like/@"@]
--    Node name satisfies /Like/ pattern.
--  [@!@ @"@/Like/@"@]
--    Node name does not satisfies /Like/ pattern.
--  [@id@ @"@/Text/@"@]
--    __Used for XML tree:__
--    Value of id attribute is equal to /Text/.
--  [@class@ @"@/Text/@"@]
--    __Used for XML tree:__
--    Space-delimited values of class attribute contains /Text/.
--  [@"@/Text/@"@ @=@ @"@/Text/@"@]
--    __Used for XML tree:__
--    Attribute value is equal to /Text/.
--

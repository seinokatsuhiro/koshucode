{-# OPTIONS_GHC -Wall #-}

-- | Decode subtree.

module Koshucode.Baala.Syntax.Subtree.Decode
  ( readSubtreeClauses,
    decodeSubtreePattern,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Symbol           as S
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.Tree             as S
import qualified Koshucode.Baala.Syntax.Subtree.Subtree  as S
import qualified Koshucode.Baala.Syntax.Subtree.Filter   as S
import qualified Koshucode.Baala.Syntax.Token.Pattern    as P
import qualified Koshucode.Baala.Syntax.Tree.Pattern     as P
import qualified Koshucode.Baala.Base.Message            as Msg

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

-- | Insert open/sep/close to subtree clause.
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

-- | Decode token trees to subtree patterns.
--
--   >>> S.withTrees decodeSubtreePattern "- \"Z1\""
--   Right [SubtreeL (SubtreeEq "Z1")]
--
--   >>> S.withTrees decodeSubtreePattern "> \"Y1\" ( - \"Z1\" )"
--   Right [SubtreeB (SubtreeEq "Y1") [SubtreeL (SubtreeEq "Z1")]]
--
--   >>> S.withTrees decodeSubtreePattern "> \"Y1\" ( - \"Z1\" | - \"Z2\" )"
--   Right [ SubtreeB (SubtreeEq "Y1") [ SubtreeL (SubtreeEq "Z1")
--                                     , SubtreeL (SubtreeEq "Z2") ]]
--
--   >>> S.withTrees decodeSubtreePattern ">> \"Y1\" ( - \"Z1\" )"
--   Right [SubtreeR (SubtreeEq "Y1") [SubtreeL (SubtreeEq "Z1")]]
--
--   >>> S.withTrees decodeSubtreePattern ">> \"Y1\" ( - A B /z \"Z1\" )"
--   Right [SubtreeR (SubtreeEq "Y1") [SubtreeL (SubtreeText ["A", "B"] (TermName EQ "z")) (SubtreeEq "Z1")]]
--
decodeSubtreePattern :: [S.Tree] -> B.Ab [S.SubtreePattern]
decodeSubtreePattern = pats where
    pats ts = pat O.<#> S.divideTreesByBar ts

    pat (P.LRaw "-" : ts) = do (ts', term) <- splitClassTerm ts
                               (f, _) <- filt ts'
                               Right $ S.SubtreeL term f
    pat (P.LRaw ">" : ts) = do (ts', term) <- splitClassTerm ts
                               S.SubtreeB term </> ts'
    pat (P.LRaw ">>" : ts) = S.SubtreeR </> ts
    pat _ = Msg.adlib "Unknown subtree pattern"

    filt []                          = Right (S.subtreeId, [])
    filt (P.LQq s : ts)              = S.subtreeEq s <+> ts
    filt (P.LRaw "?" : P.LQq s : ts) = S.subtreeKeep s <+> ts
    filt (P.LRaw "!" : P.LQq s : ts) = S.subtreeOmit s <+> ts
    filt [P.BGroup ts]               = do ps <- pats ts
                                          Right (S.subtreeId, ps)
    filt _ = Msg.adlib "Unknown subtree filter"

    k </> ts = do (f, ps) <- filt ts
                  Right $ k f ps
    f <+> ts = do (g, ps) <- filt ts
                  Right (f O.++ g, ps)

splitClassTerm :: [S.Tree] -> B.Ab ([S.Tree], S.SubtreeTerm)
splitClassTerm ts0 = loop [] ts0 where
    loop cs (P.LRaw c : ts)  = loop (c : cs) ts
    loop cs (P.LTerm n : P.LRaw "seq" : ts)
        = right cs (ts, S.SubtreeSeq (reverse cs) (S.toTermName n))
    loop cs (P.LTerm n : ts)
        = right cs (ts, S.subtreeTexts (reverse cs) (S.toTermName n))
    loop _ _ = Right (ts0, S.SubtreeNone)

    right cs result
        | null cs    = Msg.adlib "Expect one or more judgement classes"
        | otherwise  = Right result

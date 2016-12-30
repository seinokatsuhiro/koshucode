{-# OPTIONS_GHC -Wall #-}

-- | Subtree clause.

module Koshucode.Baala.Syntax.Tree.Subtree
  ( readSubtreeClauses,
  ) where

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


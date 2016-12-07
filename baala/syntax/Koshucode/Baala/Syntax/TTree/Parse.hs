{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parser for token tree.

module Koshucode.Baala.Syntax.TTree.Parse
  ( -- * Parser
    ToTrees (..),
    ttreeGroup,
    ttDoc,
    readClauseTrees,
  ) where

import qualified Text.PrettyPrint                        as P
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.TTree.Bracket    as S
import qualified Koshucode.Baala.Syntax.TTree.TokenTree  as S


-- --------------------------------------------  Parser

-- | Convert to token trees.
class ToTrees a where
    -- | Convert to list of token trees.
    toTrees :: a -> B.Ab [S.TTree]

    -- | Convert to token tree.
    toTree :: a -> B.Ab S.TTree
    toTree a = ttreeGroup <$> toTrees a

    -- | List of token trees or error.
    toTrees' :: a -> [S.TTree]
    toTrees' = B.unabort . toTrees

    -- | Token tree or error.
    toTree' :: a -> S.TTree
    toTree' = B.unabort . toTree

    -- | Pretty print token trees.
    ppTrees :: a -> IO ()
    ppTrees a = case toTrees a of
                  Left ab   -> putStrLn `mapM_` B.abortMessage [] ab
                  Right ts  -> print $ ttDoc ts

    -- | Pretty print token tree.
    ppTree :: a -> IO ()
    ppTree a = case toTrees a of
                 Left ab   -> putStrLn `mapM_` B.abortMessage [] ab
                 Right []  -> return ()
                 Right [t] -> print $ ttDoc [t]
                 Right ts  -> print $ ttDoc [ttreeGroup ts]

instance ToTrees String where
    toTrees = ttrees . S.toks

instance ToTrees [S.Token] where
    toTrees = ttrees

instance ToTrees [S.TTree] where
    toTrees = Right

-- | Parse tokens with brackets into trees.
--   Blank tokens and comments are excluded.
ttrees :: [S.Token] -> B.Ab [S.TTree]
ttrees = B.trees S.getBracketType B.BracketNone . S.prepareTokens

-- | Wrap trees in group.
ttreeGroup :: [S.TTree] -> S.TTree
ttreeGroup = B.treeWrap S.BracketGroup

-- | Get 'B.Doc' value of token trees for pretty printing.
ttDoc :: [S.TTree] -> B.Doc
ttDoc = dv where
    dv = B.pprintV . map d
    d (B.TreeL x) = B.pprint "|" B.<+> B.pprint x
    d (B.TreeB br oc xs) =
        let tag   = "<" ++ B.name br ++ ">"
            treeB = B.pprint tag B.<+> brackets oc
        in P.hang treeB 2 $ dv xs

    brackets Nothing = B.pprint ""
    brackets (Just (open, close)) =
        B.pprintH [B.pprint open, B.pprint "--", B.pprint close]

-- | Read clauses and convert to token trees.
readClauseTrees :: FilePath -> IO (B.Ab [[S.TTree]])
readClauseTrees path =
    do toks' <- S.readClauseTokens path
       return $ case toks' of
         Right toks -> mapM toTrees toks
         Left a     -> Left a


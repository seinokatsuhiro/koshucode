{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parser for token tree.

module Koshucode.Baala.Syntax.TTree.Parse
  ( -- * Parser
    ToTrees (..),
    ttreeGroup,
    ttDoc,

    -- * Split and divide
    splitTokensBy, splitTreesBy,
    divideTreesBy,
    divideTreesByBar, divideTreesByColon, divideTreesByEqual,
  ) where

import qualified Text.PrettyPrint                        as P
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.TTree.Bracket    as S
import qualified Koshucode.Baala.Syntax.TTree.TokenTree  as S
import qualified Koshucode.Baala.Syntax.Token.Pattern    as P
import qualified Koshucode.Baala.Syntax.TTree.Pattern    as P


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


-- --------------------------------------------  Split and divide

-- | Split token list by unquoted word.
--   If token list contains the word,
--   pair of /before-list/, /the-word/ and /after-list/ is returned.
--   If does not contains the word,
--   original token list is returned.
--
--   >>> let Right t = S.toks "b c"
--   >>> splitTokensBy (== "|") t
--   Left [ TText  CodePos {..} TextRaw "b"
--        , TSpace CodePos {..} 1
--        , TText  CodePos {..} TextRaw "c" ]
--
--   >>> let Right t = S.toks "a | b | c"
--   >>> splitTokensBy (== "|") t
--   Right ( [ TText CodePos {..} TextRaw "a", TSpace CodePos {..} 1]
--         , TText CodePos {..} TextRaw "|"
--         , [ TText  CodePos {..} TextRaw "b"
--           , TSpace CodePos {..} 1
--           , TText  CodePos {..} TextRaw "|"
--           , TText  CodePos {..} TextRaw "c" ] )
--
splitTokensBy :: O.Test String -> B.SplitList3e S.Token
splitTokensBy p = B.splitBy p2 where
    p2 (P.TRaw x) = p x
    p2 _          = False

raw :: O.Test String -> S.TTree -> Bool
raw p (P.LRaw w) = p w
raw _ _          = False

-- | Split token trees by quoteless token of given string.
splitTreesBy :: O.Test String -> B.SplitList3e S.TTree
splitTreesBy = B.splitBy . raw

-- | Divide token trees by quoteless token of given string.
divideTreesBy :: O.Test String -> [S.TTree] -> [[S.TTree]]
divideTreesBy = B.divideBy . raw

-- | Divide token trees by vertical bar @\"|\"@.
divideTreesByBar :: [S.TTree] -> [[S.TTree]]
divideTreesByBar = divideTreesBy (== "|")

-- | Divide token trees by colon @\":\"@.
divideTreesByColon :: [S.TTree] -> [[S.TTree]]
divideTreesByColon = divideTreesBy (== ":")

-- | Divide token trees by equal sign @\"=\"@.
divideTreesByEqual :: [S.TTree] -> [[S.TTree]]
divideTreesByEqual = divideTreesBy (== "=")


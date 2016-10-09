{-# OPTIONS_GHC -Wall #-}

-- | Parser for token tree.

module Koshucode.Baala.Syntax.TTree.Parse
  ( -- * Parser
    ttrees, ttreeGroup,
  
    -- * Abbreviation
    tt, tt1, ttPrint, ttDoc,

    -- * Split and divide
    splitTokensBy, splitTreesBy,
    divideTreesBy,
    divideTreesByBar, divideTreesByColon, divideTreesByEqual,
  ) where

import qualified Text.PrettyPrint                        as P
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.TTree.Bracket    as S
import qualified Koshucode.Baala.Syntax.TTree.TokenTree  as S


-- --------------------------------------------  Parser

-- | Parse tokens with brackets into trees.
--   Blank tokens and comments are excluded.
ttrees :: [S.Token] -> B.Ab [S.TTree]
ttrees = B.trees S.getBracketType B.BracketNone

-- | Wrap trees in group.
ttreeGroup :: S.TTreesTo S.TTree
ttreeGroup = B.treeWrap S.BracketGroup


-- --------------------------------------------  Abbreviation

-- | Convert string to token trees.
tt :: String -> B.Ab [S.TTree]
tt s = ttrees $ S.sweepToken $ S.toks s

-- | Parse string and group it.
tt1 :: String -> B.Ab S.TTree
tt1 = Right . ttreeGroup B.<=< tt

-- | Parse string and print it.
ttPrint :: String -> IO ()
ttPrint s = case tt s of
              Left msg    -> print msg
              Right trees -> do print $ ttDoc trees
                                return ()

-- | Get 'B.Doc' value of token trees for pretty printing.
ttDoc :: S.TTreesTo B.Doc
ttDoc = dv where
    dv = B.pprintV . map d
    d (B.TreeL x) = B.pprint "TreeL :" B.<+> B.pprint x
    d (B.TreeB n pp xs) =
        let treeB = B.pprintH ["TreeB", show n] B.<+> brackets pp
        in P.hang treeB 2 (dv xs)

    brackets Nothing = B.pprint "no brackets"
    brackets (Just (open, close)) = B.pprintH [B.pprint ":", B.pprint open, B.pprint close]


-- --------------------------------------------  Split and divide

-- | Split token list by unquoted word.
--   If token list contains the word,
--   pair of /before-list/, /the-word/ and /after-list/ is returned.
--   If does not contains the word,
--   original token list is returned.
--
--   >>> let Right t = S.toks "b c"
--   >>> splitTokensBy (== "|") t
--   Left [ TText  CodePt {..} TextRaw "b"
--        , TSpace CodePt {..} 1
--        , TText  CodePt {..} TextRaw "c" ]
--
--   >>> let Right t = S.toks "a | b | c"
--   >>> splitTokensBy (== "|") t
--   Right ( [ TText CodePt {..} TextRaw "a", TSpace CodePt {..} 1]
--         , TText CodePt {..} TextRaw "|"
--         , [ TText  CodePt {..} TextRaw "b"
--           , TSpace CodePt {..} 1
--           , TText  CodePt {..} TextRaw "|"
--           , TText  CodePt {..} TextRaw "c" ] )

splitTokensBy :: B.Test String -> B.SplitList3e S.Token
splitTokensBy p = B.splitBy p2 where
    p2 (S.TTextRaw _ x)  = p x
    p2 _ = False

raw :: B.Test String -> S.TTree -> Bool
raw p (S.TextLeafRaw _ w) = p w
raw _ _ = False

-- | Split token trees by quoteless token of given string.
splitTreesBy :: B.Test String -> B.SplitList3e S.TTree
splitTreesBy = B.splitBy . raw

-- | Divide token trees by quoteless token of given string.
divideTreesBy :: B.Test String -> S.TTreesTo [[S.TTree]]
divideTreesBy = B.divideBy . raw

-- | Divide token trees by vertical bar @\"|\"@.
divideTreesByBar :: S.TTreesTo [[S.TTree]]
divideTreesByBar = divideTreesBy (== "|")

-- | Divide token trees by colon @\":\"@.
divideTreesByColon :: S.TTreesTo [[S.TTree]]
divideTreesByColon = divideTreesBy (== ":")

-- | Divide token trees by equal sign @\"=\"@.
divideTreesByEqual :: S.TTreesTo [[S.TTree]]
divideTreesByEqual = divideTreesBy (== "=")


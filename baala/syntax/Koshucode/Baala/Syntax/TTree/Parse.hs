{-# OPTIONS_GHC -Wall #-}

-- | Parser for token tree.

module Koshucode.Baala.Syntax.TTree.Parse
  ( -- * Parser
    ttrees, ttreeGroup,

    -- * Divide trees
    splitTokensBy, splitTreesBy,
    divideTreesBy,
    divideTreesByBar, divideTreesByColon, divideTreesByEqual,
  
    -- * Abbreviation
    tt, tt1, ttPrint, ttDoc,
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
ttrees = B.trees S.getBracketType B.BracketNone where
    --und = map (B.undouble (== BracketGroup))

-- | Wrap trees in group.
ttreeGroup :: S.TTreesTo S.TTree
ttreeGroup = B.treeWrap S.BracketGroup


-- --------------------------------------------  Divide trees

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

splitTokensBy
    :: B.Pred String   -- ^ Tester for text token
    -> [S.Token]       -- ^ Tokens
    -> Either [S.Token] ([S.Token], S.Token, [S.Token])
       -- ^ Original-tokens or @(@before-list, the-word, after-list@)@
splitTokensBy p = B.splitBy p2 where
    p2 (S.TTextRaw _ x)  = p x
    p2 _ = False

sameText :: String -> S.TTree -> Bool
sameText w1 (S.TextLeafRaw _ w2) = w1 == w2
sameText _ _ = False

-- | Split token trees by quoteless token of given string.
splitTreesBy :: String -> [S.TTree] -> Either [S.TTree] ([S.TTree], S.TTree, [S.TTree])
splitTreesBy = B.splitBy . sameText

-- | Divide token trees by quoteless token of given string.
divideTreesBy :: String -> S.TTreesTo [[S.TTree]]
divideTreesBy = B.divideBy . sameText

-- | Divide token trees by vertical bar @\"|\"@.
divideTreesByBar :: S.TTreesTo [[S.TTree]]
divideTreesByBar = divideTreesBy "|"

-- | Divide token trees by colon @\":\"@.
divideTreesByColon :: S.TTreesTo [[S.TTree]]
divideTreesByColon = divideTreesBy ":"

-- | Divide token trees by equal sign @\"=\"@.
divideTreesByEqual :: S.TTreesTo [[S.TTree]]
divideTreesByEqual = divideTreesBy "="


-- ----------------------  Abbreviation

-- | Convert text to token trees.
tt :: String -> B.Ab [S.TTree]
tt s = do ts <- S.toks s
          ttrees $ S.sweepToken ts

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
    dv = B.docv . map d
    d (B.TreeL x) = B.doc "TreeL :" B.<+> B.doc x
    d (B.TreeB n pp xs) =
        let treeB = B.doch ["TreeB", show n] B.<+> brackets pp
        in P.hang treeB 2 (dv xs)

    brackets Nothing = B.doc "no brackets"
    brackets (Just (open, close)) = B.doch [B.doc ":", B.doc open, B.doc close]


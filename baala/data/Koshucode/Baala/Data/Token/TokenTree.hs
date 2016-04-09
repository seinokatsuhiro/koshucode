{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tree of tokens

module Koshucode.Baala.Data.Token.TokenTree
  ( -- * Token tree
    TTree,
    NamedTree, NamedTrees,
    TTreeTo, TTreesTo,
    TTreeToAb, TTreesToAb,
    ttrees,
    ttreeGroup,

    -- * Pattern
    pattern TermLeaf,
    pattern TermLeafName,
    pattern TermLeafPath,
    pattern TermLeafLocal,
    pattern TextLeaf,
    pattern TextLeafRaw,
    pattern TextLeafQ,
    pattern TextLeafQQ,
    pattern TextLeafKey,

    -- * Bracket type
    BracketType (..),
    groupOpen, groupClose,
    listOpen, listClose,
    setOpen, setClose,
    tieOpen, tieClose,
    relOpen, relClose,
    interpOpen, interpClose,
    typeOpen, typeClose,
  
    -- * Divide trees
    splitTokensBy, divideTreesBy,
    divideTreesByBar, divideTreesByColon, divideTreesByEqual,
  
    -- * Abbreviation
    tt, tt1, ttDoc, ttPrint,
  ) where

import qualified Data.Generics                        as G
import qualified Text.PrettyPrint                     as P
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Data.Token.Token     as D
import qualified Koshucode.Baala.Data.Token.TokenLine as D


-- ---------------------- Token tree

-- | Tree of tokens.
type TTree = B.CodeTree BracketType D.Token

-- | Pair of token trees and its name.
type NamedTrees = B.Named [TTree]

-- | Pair of token tree and its name.
type NamedTree = B.Named TTree

-- | Convert token tree to something.
type TTreeTo a    = TTree -> a

-- | Convert list of token tree to sometning.
type TTreesTo a   = [TTree] -> a

-- | Convert token tree to something, abortable.
type TTreeToAb a  = TTree -> B.Ab a

-- | Convert list of token tree to sometning, abortable.
type TTreesToAb a = [TTree] -> B.Ab a

-- local leaf
pattern TermLeafLocal cp v e ps = B.TreeL (D.TLocal cp v e ps)

-- term leaf
pattern TermLeaf      cp q ws   = B.TreeL (D.TTerm   cp q ws)
pattern TermLeafName  cp w      = B.TreeL (D.TTermN  cp w)
pattern TermLeafPath  cp ws     = TermLeaf cp D.TermTypePath ws

-- text leaf
pattern TextLeaf form cp w     = B.TreeL (D.TText   cp form w)
pattern TextLeafRaw   cp w     = TextLeaf D.TextRaw cp w
pattern TextLeafQ     cp w     = TextLeaf D.TextQ   cp w
pattern TextLeafQQ    cp w     = TextLeaf D.TextQQ  cp w
pattern TextLeafKey   cp w     = TextLeaf D.TextKey cp w

-- | Parse tokens with brackets into trees.
--   Blank tokens and comments are excluded.
ttrees :: [D.Token] -> B.Ab [TTree]
ttrees = B.trees getBracketType B.BracketNone where
    --und = map (B.undouble (== BracketGroup))

-- | Wrap trees in group.
ttreeGroup :: TTreesTo TTree
ttreeGroup = B.treeWrap BracketGroup


-- ----------------------  Bracket type

-- | There are nine types of brackets
data BracketType
    = BracketGroup    -- ^ Round brackets for grouping: @( E ... )@
    | BracketForm     -- ^ Round-bar brackets for form with blanks: @(| V ... | E ... |)@
    | BracketList     -- ^ Square brackets for lists: @[ C | ... ]@
    | BracketSet      -- ^ Curely braces for sets: @{ C | .... }@
    | BracketAssn     -- ^ Curely-hyphen braces for associations etc.: @{- /N C ... -}@
    | BracketRel      -- ^ Curely-equal braces for relations: @{= /N ... [ C | ... ][ C | ... ] =}@
    | BracketInterp   -- ^ Curely-bar braces for data interpretation: @{| ... /N ... |}@
    | BracketType     -- ^ Square-hyphen brackets for data type: @[- ... -]@
    | BracketUnknown  -- ^ Unknown bracket
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

getBracketType :: B.GetBracketType BracketType D.Token
getBracketType = B.bracketTable
    [ o BracketGroup   groupOpen  groupClose
    , o BracketForm    "(|"       "|)"
    , o BracketList    listOpen   listClose
    , o BracketSet     setOpen    setClose
    , o BracketAssn    tieOpen    tieClose
    , o BracketRel     relOpen    relClose
    , o BracketInterp  interpOpen interpClose
    , o BracketType    typeOpen   typeClose
    , (BracketUnknown, D.isOpenToken, D.isCloseToken)
    ] where o n a b = (n, D.isOpenTokenOf a, D.isCloseTokenOf b)

groupOpen, groupClose :: String
groupOpen    = "("
groupClose   = ")"

listOpen, listClose :: String
listOpen     = "["
listClose    = "]"

setOpen, setClose :: String
setOpen      = "{"
setClose     = "}"

tieOpen, tieClose :: String
tieOpen      = "{-"
tieClose     = "-}"

relOpen, relClose :: String
relOpen      = "{="
relClose     = "=}"

interpOpen, interpClose :: String
interpOpen   = "{|"
interpClose  = "|}"

typeOpen, typeClose :: String
typeOpen     = "[-"
typeClose    = "-]"


-- ----------------------  Divide trees

-- | Split token list by unquoted word.
--   If token list contains the word,
--   pair of /before-list/, /the-word/ and /after-list/ is returned.
--   If does not contains the word,
--   original token list is returned.
--
--   >>> splitTokensBy (== "|") . tokens $ "b c"
--   Left [ TText 1 0 "b", TSpace 2 1, TText 3 0 "c" ]
--
--   >>> splitTokensBy (== "|") . tokens $ "a | b | c"
--   Right ( [ TText 1 0 "a", TSpace 2 1 ]
--           , TText 3 0 "|"
--           , [ TSpace 4 1, TText 5 0 "b", TSpace 6 1
--             , TText 7 0 "|", TSpace 8 1, TText 9 0 "c" ] )
--
splitTokensBy
    :: B.Pred String   -- ^ Predicate
    -> [D.Token]       -- ^ Tokens
    -> Either [D.Token] ([D.Token], D.Token, [D.Token])
       -- ^ Original-tokens or @(@before-list, the-word, after-list@)@
splitTokensBy p = B.splitBy p2 where
    p2 (D.TTextRaw _ x)  = p x
    p2 _ = False

-- | Divide token trees by quoteless token of given string.
divideTreesBy :: String -> TTreesTo [[TTree]]
divideTreesBy w1 = B.divideBy p where
    p (TextLeafRaw _ w2)  = w1 == w2
    p _                   = False

-- | Divide token trees by vertical bar @\"|\"@.
divideTreesByBar :: TTreesTo [[TTree]]
divideTreesByBar = divideTreesBy "|"

-- | Divide token trees by colon @\":\"@.
divideTreesByColon :: TTreesTo [[TTree]]
divideTreesByColon = divideTreesBy ":"

-- | Divide token trees by equal sign @\"=\"@.
divideTreesByEqual :: TTreesTo [[TTree]]
divideTreesByEqual = divideTreesBy "="


-- ----------------------  Abbreviation

-- | Convert text to token trees.
tt :: String -> B.Ab [TTree]
tt s = do toks <- D.tokens (B.codeTextOf s) s
          ttrees $ D.sweepToken toks

tt1 :: String -> B.Ab TTree
tt1 = Right . ttreeGroup B.<=< tt

-- | Get 'B.Doc' value of token trees for pretty printing.
ttDoc :: TTreesTo B.Doc
ttDoc = dv where
    dv = B.docv . map d
    d (B.TreeL x) = B.doc "TreeL :" B.<+> B.doc x
    d (B.TreeB n pp xs) =
        let treeB = B.doch ["TreeB", show n] B.<+> brackets pp
        in P.hang treeB 2 (dv xs)

    brackets Nothing = B.doc "no brackets"
    brackets (Just (open, close)) = B.doch [B.doc ":", B.doc open, B.doc close]

ttPrint :: String -> IO ()
ttPrint s = case tt s of
              Left msg    -> print msg
              Right trees -> do print $ ttDoc trees
                                return ()

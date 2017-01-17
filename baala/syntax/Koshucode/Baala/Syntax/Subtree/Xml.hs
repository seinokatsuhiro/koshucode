{-# OPTIONS_GHC -Wall #-}

-- | XML tree.

module Koshucode.Baala.Syntax.Subtree.Xml
  ( -- * XML token
    XmlToken (..),
    XmlTerm,
    XmlKey (..),
    xmlTokens,

    -- * XML tree
    XmlTree,
    xmlTrees,
    xmlDecode,

    -- * Conversion
    XmlOutput,
    xmlOutput,
    xmlData,
    xmlSubtreeData,
  ) where

import qualified Text.HTML.TagSoup                       as X
import qualified Text.StringLike                         as X
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Subtext                 as U
import qualified Koshucode.Baala.Syntax.Symbol           as S
import qualified Koshucode.Baala.Syntax.Subtree.Filter   as S
import qualified Koshucode.Baala.Syntax.Subtree.Subtree  as S


-- ============================================  XML token

-- | XML token.
data XmlToken s
    = XmlOpen  s [(s, s)]   -- ^ Open tag and attributes
    | XmlClose s            -- ^ Close tag
    | XmlTerm (XmlTerm s)   -- ^ Terminal token
      deriving (Show, Eq, Ord)

instance B.GetCodePos (XmlToken s) where
    getCP  _ = B.def
    getCPs _ = []

-- | Terminal of XML tree.
type XmlTerm s = (XmlKey s, s)

-- | Key data of XML tree.
data XmlKey s
    = XmlElem s [(s, s)]   -- ^ Element node
    | XmlAttr s            -- ^ Attribute of element
    | XmlText              -- ^ Text node
    | XmlComment           -- ^ Comment node
      deriving (Show, Eq, Ord)

-- | Tokenize XML document.
--
--   >>> xmlTokens "<hello>wonderful</world>"
--   [XmlOpen "hello", XmlTerm (XmlText, "wonderful"), XmlClose "hello"]
--
--   >>> xmlTokens "<a x='xx' y='yy'>"
--   [XmlOpen "a", XmlTerm (XmlAttr "x", "xx"), XmlTerm (XmlAttr "y", "yy"), XmlClose "a"]
--
xmlTokens :: (X.StringLike s) => s -> [XmlToken s]
xmlTokens = convertTags . X.parseTags

convertTags :: (X.StringLike s) => [X.Tag s] -> [XmlToken s]
convertTags = loop [] where
    loop up (X.TagOpen n xs : tags) = XmlOpen n xs : ((attr <$> xs)
                                                      ++ loop (n : up) tags)
    loop (u : up) tags'@(X.TagClose n : tags)
                       | u == n     = XmlClose n   : loop up tags
                       | otherwise  = XmlClose u   : loop up tags'
    loop up (X.TagText s    : tags) = XmlTerm (XmlText, s) : loop up tags
    loop up (X.TagComment s : tags) = XmlTerm (XmlComment, s) : loop up tags
    loop up (_              : tags) = loop up tags
    loop up []                      = XmlClose <$> up

    attr (n, v) = XmlTerm (XmlAttr n, v)


-- ============================================  XML tree

-- | XML tree.
type XmlTree s = S.Subtree (XmlTerm s)

type XmlTree' s = B.CodeTree (XmlTerm s) (XmlToken s)

-- | Convert XML tokens to XML tree.
xmlTrees :: (Ord s, X.StringLike s) => [XmlToken s] -> B.Ab [XmlTree s]
xmlTrees toks =
    do trees <- B.codeTrees bracket B.BracketNone toks
       Right (modifyTree <$> trees)
    where
      bracket (XmlOpen n xs) = B.BracketOpen  (XmlElem n xs, n)
      bracket (XmlClose n)   = B.BracketClose (XmlElem n [], n)
      bracket _              = B.BracketNone

modifyTree :: (X.StringLike s) => XmlTree' s -> XmlTree s
modifyTree = B.treeMap (const []) y xmlUntoken where
    y (Just (t, _))  = xmlUntoken t
    y _              = (XmlComment, X.empty)

xmlUntoken :: XmlToken s -> XmlTerm s
xmlUntoken (XmlOpen  n xs) = (XmlElem n xs, n)
xmlUntoken (XmlClose n)    = (XmlElem n [], n)
xmlUntoken (XmlTerm  z)    = z

-- | Decode XML trees.
--
--   >>> B.printTrees O.# B.abortLeft (xmlDecode "<hello>wonderful</world>")
--   > () (XmlElem "hello","hello")
--     - (XmlText,"wonderful")
--
--   >>> B.printTrees O.# B.abortLeft (xmlDecode "<a x='xx' y='yy'>")
--   > () (XmlElem "a","a")
--     - (XmlAttr "x","xx")
--     - (XmlAttr "y","yy")
--
xmlDecode :: (Ord s, X.StringLike s) => s -> B.Ab [XmlTree s]
xmlDecode = xmlTrees . xmlTokens


-- ============================================  Conversion

-- | XML output tree.
type XmlOutput s = S.SubtreeOutput (XmlTerm s)

-- | Calculate subtree of XML tree.
xmlOutput :: [S.SubtreePattern] -> [XmlTree String] -> [XmlOutput String]
xmlOutput = S.subtreeBy xmlTest

-- | Test XML term matches selection rule.
xmlTest :: S.Sivmap -> XmlTerm String -> Bool
xmlTest (S.SivmapId)        _  = True
xmlTest (S.SivmapEq s)      t  = s == xmlString t
xmlTest (S.SivmapKeep _ e)  t  =       U.sivMatchExpr e $ xmlString t
xmlTest (S.SivmapOmit _ e)  t  = not $ U.sivMatchExpr e $ xmlString t
xmlTest (S.SivmapChain f g) t  = xmlTest f t && xmlTest g t
xmlTest (S.SivmapAssoc n v) (XmlElem _ xs, _) =
    case lookup n xs of
      Nothing -> True
      Just v' -> v == v'
xmlTest (S.SivmapAssoc _ _) _ = False

xmlString :: XmlTerm String -> String
xmlString (XmlElem s _ , _) = s
xmlString (XmlAttr s   , _) = s
xmlString (XmlText     , _) = "@text"
xmlString (XmlComment  , _) = "@comment"

-- | Extract data from XML output tree.
xmlData :: XmlOutput String -> [S.RawJudge O.Value]
xmlData = S.subtreeData . xmlValue

-- | Convert XML output to value tree.
xmlValue :: XmlOutput String -> S.SubtreeValue
xmlValue = snd . loop 1 where
    loop i (B.TreeL z)      = (i + 1, B.TreeL (value i z))
    loop i (B.TreeB b y xs) = let (i', xs') = branch (i + 1) [] xs
                              in (i', B.TreeB b (value i y) xs')

    value _ (S.SubtreeNone, _)            = (S.SubtreeNone, O.VEmpty)
    value _ (S.SubtreeText cs n, (_, s))  = (S.SubtreeText cs n, O.VStr $ O.trimBoth s)
    value i (S.SubtreeSeq  cs n, _)       = (S.SubtreeSeq  cs n, O.VInt i)

    branch i xs' [] = (i, reverse xs')
    branch i xs' (x : xs) = case loop i x of
                              (i', x') -> branch i' (x' : xs') xs

-- | Calculate XML subtree and extract data.
xmlSubtreeData :: [S.SubtreePattern] -> [XmlTree String] -> [S.RawJudge O.Value]
xmlSubtreeData ps ts = xmlData O.<++> xmlOutput ps ts

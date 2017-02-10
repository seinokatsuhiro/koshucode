{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attributes of relmap operator.

module Koshucode.Baala.Syntax.Attr.Attr
  ( -- * Attribute layout
    AttrLayout (..),
    AttrBranch (..),
    AttrParaSpec,
    AttrUsage,
    attrUsages,
    attrUsageString,
  
    -- * Attribute parameter
    AttrPara, AttrParaze,
    attrPara, attrParaBy,
    maybeSingleHyphen,
    maybeDoubleHyphen,

    -- * Parse
    ToAttrLayout (..),
  ) where

import qualified Data.List                              as Ls
import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Para            as S
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Tree            as S
import qualified Koshucode.Baala.Syntax.Attr.AttrName   as S
import qualified Koshucode.Baala.Syntax.Pattern         as P
import qualified Koshucode.Baala.Syntax.Attr.Message    as Msg


-- ----------------------  Attribute layout

-- | Parameter specification for attribute name.
type AttrParaSpec = S.ParaSpec S.AttrName

-- | Usage text of attribute layout.
type AttrUsage = String

-- | Attribute layout.
data AttrLayout = AttrLayout [(Maybe S.ParaTag, AttrBranch)]
                  deriving (Show)

-- | Single layout.
data AttrBranch = AttrBranch
    { attrUsage       :: AttrUsage          -- ^ Usage description
    , attrParaSpec    :: AttrParaSpec       -- ^ Parameter specification
    , attrClassifier  :: O.Map S.AttrName   -- ^ Attribute classifier
    }

instance Show AttrBranch where
    show AttrBranch {..} = "AttrBranch (" ++ show attrParaSpec ++ ")"

-- | List of attribute usages.
attrUsages :: AttrLayout -> [AttrUsage]
attrUsages (AttrLayout ls) = (attrUsage . snd) <$> ls

-- | Concatenated attribute usage.
attrUsageString :: AttrLayout -> String
attrUsageString = Ls.intercalate " | " . attrUsages

-- ----------------------  Attribute sorter

-- | Attribute parameter.
type AttrPara t = S.Para S.AttrName (S.TTree t)

-- | Parameterizer for attribute of relmap operator.
type AttrParaze t = [S.TTree t] -> B.Ab (AttrPara t)

-- | Parameterize named attributes.
attrPara :: AttrParaze S.Chars
attrPara trees =
    let p = S.para byHyphen trees
    in Right $ S.paraNameAdd S.attrNameTrunk (S.paraPos p) p

-- | Parameterize attributes by its layout.
attrParaBy :: AttrLayout -> AttrParaze S.Chars
attrParaBy lay = attrMatch lay O.#. attrPara

-- | Match parameter with its layout.
--   See 'paraChoose' in ParaSpec module.
attrMatch :: AttrLayout -> B.AbMap (AttrPara t)
attrMatch (AttrLayout branches) p = loop [] branches where
    loop us [] = Msg.unexpAttrMulti $ reverse us
    loop us ((tag, b) : bs) =
        case branch b of
          (usage, Left unmatch) -> loop ((usage, (O.tString . S.attrNameCode) <$> unmatch) : us) bs
          (_, Right p') -> case tag of
                             Nothing -> Right p'
                             Just t  -> Right $ p' { S.paraTags = t : S.paraTags p' }

    branch AttrBranch { attrUsage      = usage
                      , attrParaSpec   = spec
                      , attrClassifier = classify } =
        (usage, S.paraMatch spec $ S.paraNameMapKeys classify p)


-- ----------------------  Name

byHyphen :: S.Tree -> Maybe S.AttrName
byHyphen = fmap S.AttrNormal . maybeSingleHyphen

-- | Take out hyphened text (like @"-x"@) from token tree.
maybeSingleHyphen :: (O.Textual t) => S.TTree t -> Maybe t
maybeSingleHyphen (P.LAtt1 n) = Just n
maybeSingleHyphen _           = Nothing

-- | Take out double-hyphened text (like @"--xyz"@) from token tree.
maybeDoubleHyphen :: (O.Textual t) => S.TTree t -> Maybe t
maybeDoubleHyphen (P.LAtt2 n) = Just n
maybeDoubleHyphen _           = Nothing


-- --------------------------------------------  Parser

-- | Convertible to attribute layout.
--
--   * __@-@Word__        — Normal attribute
--   * __@-@Word@/@__     — Relmap attribute
--   * __@-@Word@/^@__    — Local relmap attribute
--
--   >>> toAttrLayout "-a"
--   AttrLayout [(Nothing, AttrBranch (
--     ParaSpec { paraSpecPos = ParaItem 1 [AttrNormal "a"]
--              , paraSpecReqP = [AttrNormal "a"]
--              , paraSpecOptP = [], paraSpecReqN = []
--              , paraSpecOptN = [AttrNormal "@trunk"]
--              , paraSpecFirst = [], paraSpecLast = [], paraSpecMulti = [] }
--              ))]
--
class ToAttrLayout a where
    toAttrLayout :: a -> AttrLayout

instance ToAttrLayout AttrLayout where
    toAttrLayout = id

instance ToAttrLayout String where
    toAttrLayout = toAttrLayout . B.divide '|'

instance ToAttrLayout [String] where
    toAttrLayout = toAttrLayout . map addUsage where
                               addUsage spec = ("", spec)

instance ToAttrLayout [(AttrUsage, String)] where
    toAttrLayout us =
        AttrLayout $ map toBranch (S.parseParaSingle O.<$$> us)

toBranch :: (AttrUsage, (Maybe S.ParaTag, S.ParaSpec String)) -> (Maybe S.ParaTag, AttrBranch)
toBranch (usage, (tag, spec)) = (tag, attrBranch usage $ trunk $ fmap attrName spec) where
    trunk spec' = spec' { S.paraSpecOptN = S.attrNameTrunk : S.paraSpecOptN spec' }

-- | Construct attribute branch.
attrBranch :: AttrUsage -> AttrParaSpec -> AttrBranch
attrBranch usage spec = AttrBranch { attrUsage      = usage
                                   , attrParaSpec   = spec
                                   , attrClassifier = attrClassify spec }

-- | Set type of attribute name.
attrClassify :: AttrParaSpec -> O.Map S.AttrName
attrClassify spec n = n' where
    n' = case lookup (S.attrNameText n) pairs of
           Just k  -> k
           Nothing -> n

    pairs   :: [(S.Chars, S.AttrName)]
    pairs   = map pair $ S.paraSpecNames spec
    pair k  = (S.attrNameText k, k)

attrName :: (O.Textual t) => t -> S.AttrName
attrName = name . reverse . unhyphen . O.tString where
    name (O.cut2 -> O.Jp2 '^' '/' n) = S.AttrRelmapLocal  $ rev n
    name (O.cut  -> O.Jp '/' n)      = S.AttrRelmapNormal $ rev n
    name n                            = S.AttrNormal       $ rev n

    rev = S.stringChars . reverse

unhyphen :: (O.Textual t) => t -> t
unhyphen (O.cut -> O.Jp '-' n) = n
unhyphen n = S.paraBug "no hyphen" $ O.tString n


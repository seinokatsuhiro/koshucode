{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attributes of relmap operator.

module Koshucode.Baala.Syntax.Attr.Attr
  ( -- * Attribute layout
    AttrParaSpec,
    AttrLayout (..),
    AttrBranch (..),
    attrBranch,
  
    -- * Attribute parameter
    AttrPara, AttrParaze,
    attrPara, attrParaBy,
    maybeSingleHyphen,
    maybeDoubleHyphen,
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax.Para           as S
import qualified Koshucode.Baala.Syntax.Token          as S
import qualified Koshucode.Baala.Syntax.Attr.AttrName  as S
import qualified Koshucode.Baala.Syntax.Attr.Message   as Msg


-- ----------------------  Attribute layout

-- | Parameter specification for attribute name.
type AttrParaSpec = S.ParaSpec S.AttrName

-- | Attribute layout.
data AttrLayout = AttrLayout [(Maybe String, AttrBranch)]
                  deriving (Show)

-- | Single layout.
data AttrBranch = AttrBranch
    { attrParaSpec    :: AttrParaSpec       -- ^ Parameter specification
    , attrClassifier  :: B.Map S.AttrName   -- ^ Attribute classifier
    }

instance Show AttrBranch where
    show AttrBranch {..} = "AttrBranch (" ++ show attrParaSpec ++ ")"

-- | Construct attribute branch.
attrBranch :: AttrParaSpec -> AttrBranch
attrBranch spec = AttrBranch spec $ attrClassify spec

-- | Set type of attribute name.
attrClassify :: AttrParaSpec -> B.Map S.AttrName
attrClassify spec n = n' where
    n' = case lookup (S.attrNameText n) pairs of
           Just k  -> k
           Nothing -> n

    pairs   :: [(String, S.AttrName)]
    pairs   = map pair $ S.paraSpecNames spec
    pair k  = (S.attrNameText k, k)


-- ----------------------  Attribute sorter

-- | Attribute parameter.
type AttrPara = S.Para S.AttrName S.TTree

-- | Parameterizer for attribute of relmap operator.
type AttrParaze = [S.TTree] -> B.Ab AttrPara

-- | Parameterize named attributes.
attrPara :: AttrParaze
attrPara trees =
    let p = S.para byHyphen trees
    in Right $ S.paraNameAdd S.attrNameTrunk (S.paraPos p) p

-- | Parameterize attributes by its layout.
attrParaBy :: AttrLayout -> AttrParaze
attrParaBy lay = attrPara B.>=> attrMatch lay

-- | Match parameter with its layout.
attrMatch :: AttrLayout -> B.AbMap AttrPara
attrMatch (AttrLayout branches) p = loop [] branches where
    loop us [] = Msg.unexpAttrMulti $ map (fmap S.attrNameCode) $ reverse us
    loop us ((tag, b) : bs) =
        case branch b of
          Left u   -> loop (u:us) bs
          Right p' -> case tag of
                        Nothing -> Right p'
                        Just t  -> Right $ p' { S.paraTags = t : S.paraTags p' }

    branch (AttrBranch spec classify) =
        S.paraMatch spec $ S.paraNameMapKeys classify p


-- ----------------------  Name

byHyphen :: S.TTreeTo (Maybe S.AttrName)
byHyphen = fmap S.AttrNormal . maybeSingleHyphen

-- | Take out hyphened text (like @"-x"@) from token tree.
maybeSingleHyphen :: S.TTreeTo (Maybe String)
maybeSingleHyphen (S.TextLeafAttr _ n)      = Just n
maybeSingleHyphen _                         = Nothing

-- | Take out double-hyphened text (like @"--xyz"@) from token tree.
maybeDoubleHyphen :: S.TTreeTo (Maybe String)
maybeDoubleHyphen (S.TextLeafAttr2 _ n)     = Just n
maybeDoubleHyphen _                         = Nothing


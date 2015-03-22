{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Lexmap trees

module Koshucode.Baala.Core.Lexmap.LexmapTrees
  ( -- * Type and constructor
    LexmapTrees (..), consLexmapTrees,

    -- * Parameter of token trees
    TTreePara,
    ttreePara1, ttreePara2,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Lexmap.AttrEd   as C
import qualified Koshucode.Baala.Core.Lexmap.Attr     as C
import qualified Koshucode.Baala.Core.Message         as Msg


-- ----------------------  Type and constructor

data LexmapTrees = LexmapTrees
    { lexmapTrees   :: [B.TTree]
    , lexmapPara    :: TTreePara
    , lexmapAttrEd  :: C.AttrEd
    } deriving (Show, Eq, Ord)

clauseAttrType :: B.ParaType String
clauseAttrType = B.paraType `B.paraMin` 0 `B.paraOpt` ["attr"]

consLexmapTrees :: TTreePara -> B.Ab LexmapTrees
consLexmapTrees para =
    do case B.paraUnmatch para clauseAttrType of
         Nothing -> Right ()
         Just u  -> Msg.adlib $ "unknown attribute: " ++ show u
       attr <- B.paraGetOpt [] para "attr"
       edit <- C.consAttrEd attr
       let body = B.paraPos para
       Right $ LexmapTrees body para edit


-- ----------------------  Parameter of token trees

-- | Token Tree parameter.
type TTreePara = B.Para B.TTree

-- | Make token tree parameter with single-hyphen names.
ttreePara1 :: [B.Token] -> B.Ab TTreePara
ttreePara1 = ttreeParaBy C.maybeSingleHyphen

-- | Make token tree parameter with double-hyphen names.
ttreePara2 :: [B.Token] -> B.Ab TTreePara
ttreePara2 = ttreeParaBy maybeDoubleHyphen

ttreeParaBy :: B.TTreeTo (Maybe String) -> [B.Token] -> B.Ab TTreePara
ttreeParaBy f toks =
    do trees <- B.ttrees toks
       Right $ B.para f trees

maybeDoubleHyphen :: B.TTreeTo (Maybe String)
maybeDoubleHyphen (B.TextLeafRaw _ ('-' : '-' : n))  = Just n
maybeDoubleHyphen _                                  = Nothing


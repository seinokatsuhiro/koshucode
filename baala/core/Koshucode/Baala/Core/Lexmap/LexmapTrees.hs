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
import qualified Koshucode.Baala.Syntax               as D
import qualified Koshucode.Baala.Data.Message         as Msg


-- ----------------------  Type and constructor

data LexmapTrees = LexmapTrees
    { lexmapTrees   :: [D.TTree]
    , lexmapPara    :: TTreePara
    , lexmapAttrEd  :: D.AttrEd
    } deriving (Show, Eq, Ord)

clauseAttrType :: D.ParaType String
clauseAttrType = D.paraType `D.paraMin` 0 `D.paraOpt` ["attr"]

consLexmapTrees :: TTreePara -> B.Ab LexmapTrees
consLexmapTrees para =
    do case D.paraUnmatch para clauseAttrType of
         Nothing -> Right ()
         Just u  -> Msg.adlib $ "unknown attribute: " ++ show u
       attr <- D.paraGetOpt [] para "attr"
       edit <- D.consAttrEd attr
       let body = D.paraPos para
       Right $ LexmapTrees body para edit


-- ----------------------  Parameter of token trees

-- | Token Tree parameter.
type TTreePara = D.Para D.TTree

-- | Make token tree parameter with single-hyphen names.
ttreePara1 :: [D.Token] -> B.Ab TTreePara
ttreePara1 = ttreeParaBy D.maybeSingleHyphen

-- | Make token tree parameter with double-hyphen names.
ttreePara2 :: [D.Token] -> B.Ab TTreePara
ttreePara2 = ttreeParaBy D.maybeDoubleHyphen

ttreeParaBy :: D.TTreeTo (Maybe String) -> [D.Token] -> B.Ab TTreePara
ttreeParaBy f toks =
    do trees <- D.ttrees toks
       Right $ D.para f trees


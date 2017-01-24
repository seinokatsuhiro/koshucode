{-# OPTIONS_GHC -Wall #-}

-- | Lexmap trees

module Koshucode.Baala.Core.Lexmap.LexmapTrees
  ( -- * Type and constructor
    LexmapTrees (..), consLexmapTrees,

    -- * Parameter of token trees
    TTreePara,
    ttreePara1, ttreePara2,
  ) where

import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax               as S
import qualified Koshucode.Baala.Data.Message         as Msg


-- ----------------------  Type and constructor

-- | Tree representation of lexmap.
data LexmapTrees = LexmapTrees
    { lexmapTrees   :: [S.Tree]    -- ^ Positional attribute.
    , lexmapPara    :: TTreePara String   -- ^ Parameter
    , lexmapAttrEd  :: S.AttrEd    -- ^ Attribute editor
    } deriving (Show, Eq, Ord)

clauseAttrType :: S.ParaSpec String
clauseAttrType = S.paraSpec $ S.paraMin 0 . S.paraOpt ["attr"]

-- | Construct lexmap tree.
consLexmapTrees :: TTreePara String -> B.Ab LexmapTrees
consLexmapTrees para =
    do para' <- case S.paraMatch clauseAttrType para of
         Right p' -> Right p'
         Left u   -> Msg.adlib $ "unknown attribute: " ++ show u
       attr <- S.paraGetOpt [] para' "attr"
       edit <- S.consAttrEd attr
       let body = S.paraPos para'
       Right $ LexmapTrees body para' edit


-- ----------------------  Parameter of token trees

-- | Token tree parameter.
type TTreePara t = S.Para t (S.TTree t)

-- | Make token tree parameter with single-hyphen names.
ttreePara1 :: (O.Textual t, S.ToTermName t) => [S.TToken t] -> B.Ab (TTreePara t)
ttreePara1 = ttreeParaBy S.maybeSingleHyphen

-- | Make token tree parameter with double-hyphen names.
ttreePara2 :: (O.Textual t, S.ToTermName t) => [S.TToken t] -> B.Ab (TTreePara t)
ttreePara2 = ttreeParaBy S.maybeDoubleHyphen

ttreeParaBy :: (O.Textual t, S.ToTermName t) =>
  (S.TTree t -> Maybe t) -> [S.TToken t] -> B.Ab (TTreePara t)
ttreeParaBy f toks =
    do trees <- S.toTrees toks
       Right $ S.para f trees


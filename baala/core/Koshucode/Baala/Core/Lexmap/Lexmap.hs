{-# OPTIONS_GHC -Wall #-}

-- | Lexical relmap.

module Koshucode.Baala.Core.Lexmap.Lexmap
  ( -- * Data types
    Lexmap (..),
    LexmapType (..),
    RopName,

    -- * Functions
    lexName,
    lexAddMessage,
    lexMessageList,
    lexAttrTree,
  ) where

import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Syntax       as S

-- | Intermediate data that represents use of relmap operator.
--   Lexmap is constructed from a list of 'B.TTree',
--   and generic relmap is constructed from a lexmap.
data Lexmap = Lexmap
    { lexType      :: LexmapType    -- ^ Type of lexmap
    , lexToken     :: S.Token       -- ^ Token of operator
    , lexAttr      :: S.AttrPara    -- ^ Attribute of relmap operation
    , lexSubmap    :: [(String, Lexmap)]   -- ^ Submaps in the attribute
    , lexMessage   :: [String]      -- ^ Messages on lexmap
    } deriving (Show, Eq, Ord)

-- | Type of lexmap.
data LexmapType
    = LexmapBase      -- ^ Relmap of built-in operator
    | LexmapDerived   -- ^ Derived or user-defined relmap
    | LexmapLocal     -- ^ Local relation reference like @^/r@ or @^r@
      deriving (Show, Eq, Ord)

instance B.CodePtr Lexmap where
    codePtList = B.codePtList . lexToken

-- | Empty base lexmap.
instance B.Default Lexmap where
    def = Lexmap { lexType     = LexmapBase
                 , lexToken    = S.textToken ""
                 , lexAttr     = B.def
                 , lexSubmap   = []
                 , lexMessage  = [] }

-- | Name of relmap operator.
type RopName = String

-- | Attribute of relmap operation.
lexAttrTree :: Lexmap -> [S.AttrTree]
lexAttrTree = map (B.mapSnd head) . S.paraNameList . lexAttr

-- | Name of relmap operator
lexName :: Lexmap -> RopName
lexName = S.tokenContent . lexToken

-- | Add message to lexmap.
lexAddMessage :: String -> O.Map Lexmap
lexAddMessage msg lx = lx { lexMessage = msg : lexMessage lx }

-- | Get message list from lexmap.
lexMessageList :: Lexmap -> [String]
lexMessageList Lexmap { lexToken = tok, lexMessage = msg }
    | null msg  = []
    | otherwise = msg ++ src
    where src = map (("  " ++) . fst) $ B.codePtDisplay ("", pt)
          pt  = head $ B.codePtList tok


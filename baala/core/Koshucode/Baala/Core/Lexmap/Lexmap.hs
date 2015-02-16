{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Lexical relmap.

module Koshucode.Baala.Core.Lexmap.Lexmap
  ( -- * Data types
    Lexmap (..),
    LexmapType (..),
    RopName,

    -- * Functions
    lexBase,
    lexRopName,
    lexAddMessage,
    lexMessageList,
    lexAttrTree,
  ) where

import qualified Data.Generics                          as G
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Lexmap.AttrPos    as C
import qualified Koshucode.Baala.Core.Lexmap.Attribute  as C

-- | Intermediate data that represents use of relmap operator.
--   Lexmap is constructed from a list of 'B.TTree',
--   and generic relmap is constructed from a lexmap.
data Lexmap = Lexmap
    { lexType      :: LexmapType    -- ^ Type of lexmap
    , lexRopToken  :: B.Token       -- ^ Token of operator
    , lexAttr      :: C.AttrPara    -- ^ Attribute of relmap operation
    , lexSubmap    :: [Lexmap]      -- ^ Submaps in the attribute
    , lexNest      :: [String]      -- ^ Nested relation references
    , lexMessage   :: [String]      -- ^ Messages on lexmap
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

data LexmapType
    = LexmapBase         -- ^ Built-in relmap
    | LexmapDerived      -- ^ User-defined relmap
    | LexmapLocal        -- ^ Local relation variable
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Write Lexmap where
    write sh lx@Lexmap { lexAttr = para } =
        case B.paraAll para of
          [] -> B.writeH sh [op, "..."]
          xs -> B.writeH sh [op, show xs]
        where op = lexRopName lx

instance B.CodePtr Lexmap where
    codePtList = B.codePtList . lexRopToken

-- | Name of relmap operator.
type RopName = String

-- | Attribute of relmap operation.
lexAttrTree :: Lexmap -> [C.AttrTree]
lexAttrTree = map (B.mapSnd head) . B.paraNameList . lexAttr

-- | Base empty lexmap.
lexBase :: Lexmap
lexBase = Lexmap { lexType      = LexmapBase
                 , lexRopToken  = B.textToken ""
                 , lexAttr      = B.paraEmpty
                 , lexSubmap    = []
                 , lexNest      = []
                 , lexMessage   = [] }

-- | Name of relmap operator
lexRopName :: Lexmap -> RopName
lexRopName = B.tokenContent . lexRopToken

lexAddMessage :: String -> B.Map Lexmap
lexAddMessage msg lx = lx { lexMessage = msg : lexMessage lx }

lexMessageList :: Lexmap -> [String]
lexMessageList Lexmap { lexRopToken = tok, lexMessage = msg }
    | null msg  = []
    | otherwise = msg ++ src
    where src = map (("  " ++) . fst) $ B.codePtDisplay ("", pt)
          pt  = head $ B.codePtList tok


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
    lexName,
    lexAddMessage,
    lexMessageList,
    lexAttrTree,
  ) where

import qualified Data.Generics                          as G
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Data                   as D
import qualified Koshucode.Baala.Core.Lexmap.AttrPos    as C
import qualified Koshucode.Baala.Core.Lexmap.Attr       as C

-- | Intermediate data that represents use of relmap operator.
--   Lexmap is constructed from a list of 'B.TTree',
--   and generic relmap is constructed from a lexmap.
data Lexmap = Lexmap
    { lexType      :: LexmapType    -- ^ Type of lexmap
    , lexToken     :: D.Token       -- ^ Token of operator
    , lexAttr      :: C.AttrPara    -- ^ Attribute of relmap operation
    , lexSubmap    :: [Lexmap]      -- ^ Submaps in the attribute
    , lexMessage   :: [String]      -- ^ Messages on lexmap
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

data LexmapType
    = LexmapBase         -- ^ Built-in relmap
    | LexmapDerived      -- ^ User-defined relmap
    | LexmapLocal        -- ^ Local relation reference
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Write Lexmap where
    writeDocWith sh lx@Lexmap { lexAttr = para } =
        case D.paraAll para of
          [] -> B.writeH sh [op, "..."]
          xs -> B.writeH sh [op, show xs]
        where op = lexName lx

instance B.CodePtr Lexmap where
    codePtList = B.codePtList . lexToken

-- | Name of relmap operator.
type RopName = String

-- | Attribute of relmap operation.
lexAttrTree :: Lexmap -> [C.AttrTree]
lexAttrTree = map (B.mapSnd head) . D.paraNameList . lexAttr

-- | Empty base lexmap.
lexBase :: Lexmap
lexBase = Lexmap { lexType     = LexmapBase
                 , lexToken    = D.textToken ""
                 , lexAttr     = D.paraEmpty
                 , lexSubmap   = []
                 , lexMessage  = [] }

-- | Name of relmap operator
lexName :: Lexmap -> RopName
lexName = D.tokenContent . lexToken

lexAddMessage :: String -> B.Map Lexmap
lexAddMessage msg lx = lx { lexMessage = msg : lexMessage lx }

lexMessageList :: Lexmap -> [String]
lexMessageList Lexmap { lexToken = tok, lexMessage = msg }
    | null msg  = []
    | otherwise = msg ++ src
    where src = map (("  " ++) . fst) $ B.codePtDisplay ("", pt)
          pt  = head $ B.codePtList tok


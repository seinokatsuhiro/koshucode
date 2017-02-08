{-# OPTIONS_GHC -Wall #-}

-- | Lexical relmap.

module Koshucode.Baala.Core.Lexmap.Lexmap
  ( -- * Data types
    Lexmap, TLexmap (..),
    LexmapType (..),
    RopName,

    -- * Functions
    lexName, lexLocalRef,
    lexAddMessage,
    lexMessageList,
    lexAttrTree,
  ) where

import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Syntax       as S

-- | String version of 'TLexmap'.
type Lexmap = TLexmap S.Chars

-- | Intermediate data that represents use of relmap operator.
--   Lexmap is constructed from a list of 'B.TTree',
--   and generic relmap is constructed from a lexmap.
data TLexmap t = Lexmap
    { lexType      :: LexmapType            -- ^ Type of lexmap
    , lexToken     :: S.TToken t            -- ^ Token of operator
    , lexAttr      :: S.AttrPara t          -- ^ Attribute of relmap operation
    , lexSubmap    :: [(String, TLexmap t)] -- ^ Submaps in the attribute
    , lexMessage   :: [String]              -- ^ Messages on lexmap
    } deriving (Show, Eq, Ord)

-- | Type of lexmap.
data LexmapType
    = LexmapBase      -- ^ Relmap of built-in operator
    | LexmapDerived   -- ^ Derived or user-defined relmap
    | LexmapLocal     -- ^ Local relation reference like @^/r@ or @^r@
      deriving (Show, Eq, Ord)

instance (O.Textual t) => B.GetCodePos (TLexmap t) where
    getCPs = B.getCPs . lexToken

instance B.CsGetCP TLexmap where
    csGetCPs = B.csGetCPs . lexToken

-- | Empty base lexmap.
instance (O.Textual t) => B.Default (TLexmap t) where
    def = Lexmap { lexType     = LexmapBase
                 , lexToken    = S.rawTextToken O.tEmpty
                 , lexAttr     = B.def
                 , lexSubmap   = []
                 , lexMessage  = [] }

-- | Name of relmap operator.
type RopName = String

-- | Attribute of relmap operation.
lexAttrTree :: TLexmap t -> [S.AttrTree t]
lexAttrTree = map (B.mapSnd head) . S.paraNameList . lexAttr

-- | Name of relmap operator
lexName :: (O.Textual t) => TLexmap t -> RopName
lexName = S.tokenContent . lexToken

-- | Lexmap local reference.
lexLocalRef :: TLexmap t -> Maybe S.LocalRef
lexLocalRef lx = case lexToken lx of
                   S.TLocal _ ref _ _ -> Just ref
                   _                  -> Nothing

-- | Add message to lexmap.
lexAddMessage :: String -> O.Map (TLexmap t)
lexAddMessage msg lx = lx { lexMessage = msg : lexMessage lx }

-- | Get message list from lexmap.
lexMessageList :: (O.Textual t) => TLexmap t -> [String]
lexMessageList Lexmap { lexToken = tok, lexMessage = msg }
    | null msg  = []
    | otherwise = msg ++ src
    where src = map (("  " ++) . fst) $ B.cpMessageLines (cp, "")
          cp  = head $ B.getCPs tok


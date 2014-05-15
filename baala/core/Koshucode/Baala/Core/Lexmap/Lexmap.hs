{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Lexical relmap.

module Koshucode.Baala.Core.Lexmap.Lexmap
( Lexmap (..),
  LexmapType (..),
  lexOpName,
  lexAddMessage,
  lexMessageList,
) where

import qualified Data.Generics        as G
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Lexmap.Operand as C

-- | Intermediate data that represents use of relmap operator.
--   Lexmap is constructed from a list of 'B.TokenTree',
--   and generic relmap is constructed from a lexmap.
data Lexmap = Lexmap
    { lexType     :: LexmapType  -- ^ Type of lexmap
    , lexOpToken  :: B.Token     -- ^ Token of operator
    , lexOperand  :: C.Rod       -- ^ Operand of relmap operator
    , lexSubmap   :: [Lexmap]    -- ^ Submaps in the operand
    , lexMessage  :: [String]    -- ^ Messages on lexmap
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

data LexmapType
    = LexmapBase         -- ^ Built-in relmap
    | LexmapDerived      -- ^ User-defined relmap
    | LexmapWith         -- ^ @-with@ variable
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Pretty Lexmap where
    doc Lexmap { lexOpToken = opTok, lexOperand = opd } =
        case lookup "@operand" opd of
          Nothing -> B.doch [op, "..."]
          Just xs -> B.doch [op, show xs]
        where op = B.tokenContent opTok

instance B.TokenList Lexmap where
    tokenList Lexmap { lexOpToken = op } = [op]

-- | Name of relmap operator
lexOpName :: Lexmap -> String
lexOpName = B.tokenContent . lexOpToken

lexAddMessage :: String -> B.Map Lexmap
lexAddMessage msg lx = lx { lexMessage = msg : lexMessage lx }

lexMessageList :: Lexmap -> [String]
lexMessageList Lexmap { lexOpToken = tok, lexMessage = msg }
    | null msg  = []
    | otherwise = msg ++ src
    where src = map (("  " ++) . fst) $ B.codePointDisplay "" $ B.codePoint tok

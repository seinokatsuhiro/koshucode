{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Lexical relmap.

module Koshucode.Baala.Core.Relmap.Lexmap
( Lexmap (..),
  lexOpText,
) where

import qualified Data.Generics        as G
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Relmap.Operand as C

-- | Intermediate data that represents use of relmap operator.
--   Lexmap is constructed from a list of 'B.TokenTree',
--   and generic relmap is constructed from a lexmap.
data Lexmap = Lexmap
    { lexOpToken  :: B.Token     -- ^ Token of operator
    , lexOperand  :: C.Rod       -- ^ Operand of relmap operator
    , lexSubmap   :: [Lexmap]    -- ^ Submaps in the operand
    , lexUsage    :: String      -- ^ Usage description
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Pretty Lexmap where
    doc Lexmap { lexOpToken = opTok, lexOperand = opd } =
        case lookup "@operand" opd of
          Nothing -> B.doch [op, "..."]
          Just xs -> B.doch [op, show xs]
        where op = B.tokenContent opTok

instance B.TokenListing Lexmap where
    tokenListing Lexmap { lexOpToken = op } = [op]

-- | Name of relmap operator
lexOpText :: Lexmap -> String
lexOpText = B.tokenContent . lexOpToken


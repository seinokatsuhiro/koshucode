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

-- | Intermediate data that represents use of relational operator.
--
--   'Lexmap' is constructed from list of 'TokenTree',
--   and (generic) 'Relmap' is constructed from 'Lexmap'.
--
data Lexmap = Lexmap
    { lexOpToken   :: B.Token            -- ^ Operator token
    , lexOperand   :: C.RopOperandAssoc  -- ^ Operand of relmap operation
    , lexSubrelmap :: [Lexmap]           -- ^ Subrelmaps in the operand
    , lexUsage     :: String             -- ^ Usages description
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Pretty Lexmap where
    doc Lexmap { lexOpToken = opTok, lexOperand = opd } =
        case lookup "operand" opd of
          Nothing -> B.doch [op, "..."]
          Just xs -> B.doch [op, show xs]
        where op = B.tokenContent opTok

instance B.TokenListing Lexmap where
    tokenListing Lexmap { lexOpToken = op } = [op]

lexOpText :: Lexmap -> String
lexOpText = B.tokenContent . lexOpToken


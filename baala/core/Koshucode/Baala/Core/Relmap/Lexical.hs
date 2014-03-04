{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Lexical
( LexRelmap (..),
  lexOpText,
) where

import qualified Data.Generics        as G
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Relmap.Operand as C

-- | Intermediate data that represents use of relational operator.
--
--   'LexRelmap' is constructed from list of 'TokenTree',
--   and (generic) 'Relmap' is constructed from 'LexRelmap'.
--
data LexRelmap = LexRelmap
    { lexOpToken   :: B.Token            -- ^ Operator token
    , lexOperand   :: C.RopOperandAssoc  -- ^ Operand of relmap operation
    , lexSubrelmap :: [LexRelmap]    -- ^ Subrelmaps in the operand
    , lexUsage     :: String             -- ^ Usages description
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Pretty LexRelmap where
    doc LexRelmap { lexOpToken = opTok, lexOperand = opd } =
        case lookup "operand" opd of
          Nothing -> B.doch [op, "..."]
          Just xs -> B.doch [op, show xs]
        where op = B.tokenContent opTok

instance B.TokenListing LexRelmap where
    tokenListing LexRelmap { lexOpToken = op } = [op]

lexOpText :: LexRelmap -> String
lexOpText = B.tokenContent . lexOpToken


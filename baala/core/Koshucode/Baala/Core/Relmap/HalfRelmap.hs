{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.HalfRelmap
( HalfRelmap (..)
) where

import qualified Data.Generics        as G
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Relmap.Operand as C

{-| Intermediate data that represents use of relational operator.

    'HalfRelmap' is constructed from list of 'TokenTree',
    and (full) 'Relmap' is constructed from 'HalfRelmap'. -}

data HalfRelmap = HalfRelmap
    { halfUsage    :: String          -- ^ Usages description
    , halfLines    :: [B.TokenLine]   -- ^ Source information
    , halfOperator :: String          -- ^ Operator name of relmap operation
    , halfOperand  :: C.RopAssoc      -- ^ Operand of relmap operation
    , halfSubmap   :: [HalfRelmap]    -- ^ Subrelmaps in the operand
    } deriving (Show, G.Data, G.Typeable)

instance B.Pretty HalfRelmap where
    doc HalfRelmap { halfOperator = op, halfOperand = opd } =
        case lookup "operand" opd of
          Nothing -> B.doch [op, "..."]
          Just xs -> B.doch [op, show xs]


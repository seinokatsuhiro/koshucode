{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Relmap.HalfRelmap
( HalfRelmap (..)
) where

import Data.Generics
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax

-- | Intermediate data that represents use of relational operator.
-- 
--   'HalfRelmap' is constructed from list of 'TokenTree',
--   and (full) 'Relmap' is constructed from 'HalfRelmap'.

data HalfRelmap = HalfRelmap
    { halfUsage    :: [String]      -- ^ Usages description
    , halfLines    :: [SourceLine]  -- ^ Source information
    , halfOperator :: String        -- ^ Operator name of relmap operation
    , halfOperand  :: [Named [TokenTree]] -- ^ Operand of relmap operation
    , halfSubmap   :: [HalfRelmap]        -- ^ Subrelmaps in the operand
    } deriving (Show, Data, Typeable)

instance Pretty HalfRelmap where
    doc HalfRelmap { halfOperator = op, halfOperand = opd } =
        case lookup "operand" opd of
          Nothing -> text op <+> text "..."
          Just xs -> text op <+> text (tokenTreesSource xs)


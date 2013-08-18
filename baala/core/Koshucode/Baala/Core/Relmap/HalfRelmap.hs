{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.HalfRelmap
( HalfRelmap (..)
) where

import Data.Generics
import qualified Koshucode.Baala.Base as B

{-| Intermediate data that represents use of relational operator.

    'HalfRelmap' is constructed from list of 'TokenTree',
    and (full) 'Relmap' is constructed from 'HalfRelmap'. -}

data HalfRelmap = HalfRelmap
    { halfUsage    :: String                  -- ^ Usages description
    , halfLines    :: [B.TokenLine]           -- ^ Source information
    , halfOperator :: String                  -- ^ Operator name of relmap operation
    , halfOperand  :: [B.Named [B.TokenTree]] -- ^ Operand of relmap operation
    , halfSubmap   :: [HalfRelmap]            -- ^ Subrelmaps in the operand
    } deriving (Show, Data, Typeable)

instance B.Pretty HalfRelmap where
    doc HalfRelmap { halfOperator = op, halfOperand = opd } =
        case lookup "operand" opd of
          Nothing -> B.text op B.<+> B.text "..."
          Just xs -> B.text op B.<+> B.text (show xs)


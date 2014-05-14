{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Mapping of operand to operand.

module Koshucode.Baala.Core.Relmap.Rodmap
( -- * Data type
  Rodmap (..),
  rodmap,
  rodmapRun,
) where

import qualified Data.Generics                        as G
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Relmap.Operand  as C

data Rodmap
    = RodmapId
    | RodmapAdd     String [B.TokenTree]
    | RodmapOpt     String [B.TokenTree]
    | RodmapRename  String String
    | RodmapAppend  Rodmap Rodmap
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

rodmap :: [B.TokenTree] -> B.Ab Rodmap
rodmap _ = Right RodmapId

rodmapRun :: Rodmap -> B.AbMap C.Rod
rodmapRun _ rod = Right rod


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Implement
( -- * Implement
  Rop (..),
  RopFullSorter,
  RopSorter,
  ropPartNameBy,
  ropPartName,

  -- * Constructor
  RopCons,
  RopUse (..),

  -- * Relmap basis
  relmapSource,
  relmapConst,
  relmapAlias,
  relmapCalc,
  relmapConfl,

) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Relmap.Relmap     as C
import qualified Koshucode.Baala.Core.Relmap.HalfRelmap as C



-- ----------------------  Implement

{-| Implementation of relmap operator. -}
data Rop c = Rop
    { ropName       :: String         -- ^ Operator name
    , ropGroup      :: String         -- ^ Operator group
    , ropFullSorter :: RopFullSorter  -- ^ Operand sorter
    , ropCons       :: RopCons c      -- ^ Constructor of operator
    , ropUsage      :: String         -- ^ Usage of operator
    }
    
{-| Sorter for operand of relational operator.
    This soters docompose operand trees,
    and give a name to suboperand. -}
type RopFullSorter
    =  [B.TokenTree]            -- ^ Unsorted operand
    -> [B.Named [B.TokenTree]]  -- ^ Fully sorted operand

type RopSorter
    =  [B.Named [B.TokenTree]]  -- ^ Basically sorted operand
    -> [B.Named [B.TokenTree]]  -- ^ Fully sorted operand

{-| Give a name to unnamed operand. -}
ropPartNameBy :: RopFullSorter -> RopSorter
ropPartNameBy f xs =
    case lookup "" xs of
      Just x  -> f x ++ xs
      Nothing -> xs

ropPartName :: String -> RopSorter
ropPartName name = ropPartNameBy f where
    f x = [(name, x)]



-- ----------------------  Constructor

{-| Constructor of relational operator 'Relmap'.
    'Relmap' is constructed from 'HalfRelmap' and subrelmaps in it. -}
type RopCons c = RopUse c -> B.AbortTokens (C.Relmap c)

{-| Use of operator -}
data RopUse c = RopUse {
      ropHalf   :: C.HalfRelmap   -- ^ Syntactic data of operator use
    , ropSubmap :: [C.Relmap c]   -- ^ Subrelmaps
    } deriving (Show)




-- ----------------------  Relmap

{-| Retrieve relation from dataset. -}
relmapSource
    :: RopUse c      -- ^ Use of operator
    -> String        -- ^ Operator name
    -> [String]      -- ^ List of term names
    -> (C.Relmap c)  -- ^ Result relmap
relmapSource use = C.RelmapSource $ ropHalf use

{-| Constant relmap. -}
relmapConst
    :: RopUse c      -- ^ Use of operator
    -> String        -- ^ Operator name
    -> B.Rel c       -- ^ Constant relation
    -> C.Relmap c    -- ^ Result relmap
relmapConst use = C.RelmapConst $ ropHalf use

{-| Alias relmap. -}
relmapAlias
    :: RopUse c      -- ^ Use of operator
    -> C.Relmap c    -- ^ 
    -> C.Relmap c    -- ^ Result relmap
relmapAlias use = C.RelmapAlias $ ropHalf use

{-| Make a non-confluent relmap. -}
relmapCalc
    :: RopUse c       -- ^ Use of operator
    -> String         -- ^ Operator name
    -> C.RelmapSub c  -- ^ Calculation of operation
    -> C.Relmap c     -- ^ Result relmap
relmapCalc use op sub = relmapConfl use op sub C.tmapId []

{-| Make a confluent relmap. -}
relmapConfl
    :: RopUse c       -- ^ Use of operator
    -> String         -- ^ Operator name
    -> C.RelmapSub c  -- ^ Calculation of operation
    -> C.RelmapTupleMap c  -- ^ Calculation of operation
    -> [C.Relmap c]   -- ^ Subrelmaps
    -> C.Relmap c     -- ^ Result relmap
relmapConfl use = C.RelmapCalc $ ropHalf use


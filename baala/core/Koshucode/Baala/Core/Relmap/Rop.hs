{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Rop
( -- * Implement
  Rop (..),
  RopFullSorter,
  RopSorter,

  -- * Associator
  trunkId,
  trunkBy,
  trunkEnum,
  trunkElems,
  trunkUnary,
  trunkBinary,
  trunkUncons,

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
import qualified Koshucode.Baala.Core.Relmap.Relfy      as C



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
    =  [B.TokenTree]                 -- ^ Unsorted operand
    -> B.Ab [B.Named [B.TokenTree]]  -- ^ Fully sorted operand

type RopSorter
    =  [B.Named [B.TokenTree]]       -- ^ Basically sorted operand
    -> B.Ab [B.Named [B.TokenTree]]  -- ^ Fully sorted operand



-- ----------------------  Trunk associators

trunkId :: RopSorter
trunkId x = Right x

{-| Give a name to unnamed operand. -}
trunkBy :: RopFullSorter -> RopSorter
trunkBy f xs = case lookup "" xs of
                 Just x  -> Right . (++ xs) =<< f x
                 Nothing -> Right xs

trunkEnum :: RopSorter
trunkEnum       = trunkBy f where
    f xs        =  Right $ zip ns $ map B.singleton xs
    ns          =  map (('-' :) . show) [1 :: Int ..]

trunkElems :: String -> RopSorter
trunkElems a    = trunkBy f where
    f xs        = Right [ (a, xs) ]

trunkUnary :: String -> RopSorter
trunkUnary a    = trunkBy f where
    f [x]       = Right [ (a, [x]) ]
    f _         = Left $ B.AbortUndefined "unary"

trunkBinary :: String -> String -> RopSorter
trunkBinary a b = trunkBy f where
    f [x, y]    = Right [ (a, [x]), (b, [y]) ]
    f _         = Left $ B.AbortUndefined "binary"

trunkUncons :: String -> String -> RopSorter
trunkUncons a b = trunkBy f where
    f (x:xs)    = Right [ (a, [x]), (b, xs) ]
    f _         = Left $ B.AbortUndefined "uncons"



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
    :: RopUse c          -- ^ Use of operator
    -> String            -- ^ Operator name
    -> C.RelmapRelfy c   -- ^ Calculation of operation
    -> C.Relmap c        -- ^ Result relmap
relmapCalc use op relfy = relmapConfl use op relfy []

{-| Make a confluent relmap. -}
relmapConfl
    :: RopUse c          -- ^ Use of operator
    -> String            -- ^ Operator name
    -> C.RelmapRelfy c   -- ^ Calculation of operation
    -> [C.Relmap c]      -- ^ Subrelmaps
    -> C.Relmap c        -- ^ Result relmap
relmapConfl use = C.RelmapCalc $ ropHalf use


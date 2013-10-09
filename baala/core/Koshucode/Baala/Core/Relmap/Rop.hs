{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Rop
( -- * Datatype
  Rop (..),
  RopFullSorter,
  RopSorter,
  RopCons,
  RopUse (..),
  RopOperand,

  -- * Sorter
  operandNone,
  operandEnum,
  operandElems,
  operandUnary,
  operandBinary,
  operandUncons,

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



-- ----------------------  Datatype

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

{-| Constructor of relational operator 'C.Relmap'.
    'C.Relmap' is constructed from 'C.HalfRelmap' and subrelmaps in it. -}
type RopCons c = RopUse c -> B.AbortTokens (C.Relmap c)

{-| Use of operator -}
data RopUse c = RopUse {
      ropHalf   :: C.HalfRelmap   -- ^ Syntactic data of operator use
    , ropSubmap :: [C.Relmap c]   -- ^ Subrelmaps
    } deriving (Show)



-- ----------------------  Trunk sorters

type RopOperand = (RopSorter, [B.Termname], [B.Termname])

operandNone :: RopOperand
operandNone = (Right, [], [])

operandEnum :: [B.Termname] -> [B.Termname] -> RopOperand
operandEnum ks ns = (trunkBy f, ks, ns) where
    f xs          = Right $ zip names $ map B.singleton xs
    names         = map (('-' :) . show) [1 :: Int ..]

operandElems :: B.Termname -> [B.Termname] -> RopOperand
operandElems a ns = (trunkBy f, [a], ns) where
    f xs          = Right [ (a, xs) ]

operandUnary :: B.Termname -> [B.Termname] -> RopOperand
operandUnary a ns = (trunkBy f, [a], ns) where
    f [x]         = Right [ (a, [x]) ]
    f _           = Left $ B.AbortOpeandUnmatch "unary"

operandBinary :: B.Termname -> B.Termname -> [B.Termname] -> RopOperand
operandBinary a b ns = (trunkBy f, [a,b], ns) where
    f [x, y]         = Right [ (a, [x]), (b, [y]) ]
    f _              = Left $ B.AbortOpeandUnmatch "binary"

operandUncons :: B.Termname -> B.Termname -> [B.Termname] -> RopOperand
operandUncons a b ns = (trunkBy f, [a,b], ns) where
    f (x:xs)         = Right [ (a, [x]), (b, xs) ]
    f _              = Left $ B.AbortOpeandUnmatch "uncons"

{-| Give a name to unnamed operand. -}
trunkBy :: RopFullSorter -> RopSorter
trunkBy f xs = case lookup "" xs of
                 Just x  -> Right . (++ xs) =<< f x
                 Nothing -> Right xs



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


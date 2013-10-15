{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Rop
( -- * Datatype
  Rop (..),
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
import qualified Koshucode.Baala.Core.Relmap.HalfRelmap as C
import qualified Koshucode.Baala.Core.Relmap.Operand    as C
import qualified Koshucode.Baala.Core.Relmap.Relfy      as C
import qualified Koshucode.Baala.Core.Relmap.Relmap     as C



-- ----------------------  Datatype

{-| Implementation of relmap operator. -}
data Rop c = Rop
    { ropName       :: String           -- ^ Operator name
    , ropGroup      :: String           -- ^ Operator group
    , ropFullSorter :: C.RopFullSorter  -- ^ Operand sorter
    , ropCons       :: RopCons c        -- ^ Constructor of operator
    , ropUsage      :: String           -- ^ Usage of operator
    }

{-| Constructor of relational operator 'C.Relmap'.
    'C.Relmap' is constructed from 'C.HalfRelmap' and subrelmaps in it. -}
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


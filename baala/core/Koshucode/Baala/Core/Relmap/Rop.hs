{-# OPTIONS_GHC -Wall #-}

{-| Implementation of relmap operators. -}

module Koshucode.Baala.Core.Relmap.Rop
( -- * Datatype
  Rop (..),
  RopUse (..),
  RopCons,

  -- * Relmap basis
  relmapSource,
  relmapConst,
  relmapAlias,
  relmapCalc,
  relmapRelfy,
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

{-| Use of relmap operator -}
data RopUse c = RopUse {
      ropHalf      :: C.HalfRelmap   -- ^ Syntactic data of operator use
    , ropSubrelmap :: [C.Relmap c]   -- ^ Subrelmaps
    } deriving (Show)

{-| Constructor of relmap operator 'C.Relmap'. -}
type RopCons c = RopUse c -> B.Ab (C.Relmap c)



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
relmapAlias :: RopUse c -> C.Relmap c -> C.Relmap c
relmapAlias use = C.RelmapAlias $ ropHalf use

{-| Make a non-confluent relmap. -}
relmapCalc
    :: RopUse c          -- ^ Use of operator
    -> String            -- ^ Operator name
    -> C.RelmapRelfy c   -- ^ Calculation of operation
    -> C.Relmap c        -- ^ Result relmap
relmapCalc use op relfy = relmapConfl use op relfy []

relmapRelfy :: RopUse c -> String -> (B.Relhead -> B.Ab (C.Relfy c)) -> C.Relmap c
relmapRelfy use name f = relmapCalc use name $ const f

{-| Make a confluent relmap. -}
relmapConfl
    :: RopUse c          -- ^ Use of operator
    -> String            -- ^ Operator name
    -> C.RelmapRelfy c   -- ^ Calculation of operation
    -> [C.Relmap c]      -- ^ Subrelmaps
    -> C.Relmap c        -- ^ Result relmap
relmapConfl use = C.RelmapCalc $ ropHalf use


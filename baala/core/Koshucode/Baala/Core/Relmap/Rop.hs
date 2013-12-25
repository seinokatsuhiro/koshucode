{-# OPTIONS_GHC -Wall #-}

{-| Implementation of relmap operators. -}

module Koshucode.Baala.Core.Relmap.Rop
( -- * Datatypes
  Rop (..),
  RopUse (..),
  RopCons,

  -- * Basic constructors
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

{-| Implementation of relmap operator -}
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

{-| Constructor of relmap operator -}
type RopCons c = RopUse c -> B.Ab (C.Relmap c)



-- ----------------------  Relmap

{-| Retrieve relation from dataset. -}
relmapSource :: RopUse c -> B.JudgePattern -> [B.Termname] -> (C.Relmap c)
relmapSource = C.RelmapSource . ropHalf

{-| Constant relmap. -}
relmapConst :: RopUse c -> B.Rel c -> C.Relmap c
relmapConst = C.RelmapConst . ropHalf

{-| Alias for relmap. -}
relmapAlias :: RopUse c -> C.Relmap c -> C.Relmap c
relmapAlias = C.RelmapAlias . ropHalf

{-| Make a non-confluent relmap. -}
relmapCalc :: RopUse c -> C.RelmapCalcRelfy c -> C.Relmap c
relmapCalc use relfy = relmapConfl use (const relfy) []

{-| Make a confluent relmap. -}
relmapConfl :: RopUse c -> C.RelmapConflRelfy c -> [C.Relmap c] -> C.Relmap c
relmapConfl = C.RelmapCalc . ropHalf


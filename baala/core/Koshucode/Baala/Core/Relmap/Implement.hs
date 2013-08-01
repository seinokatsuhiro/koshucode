{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Implement
( -- * Implement
  Rop (..),
  RopParser,
  RopParser',

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

import Koshucode.Baala.Base
import Koshucode.Baala.Core.Relmap.Relmap
import Koshucode.Baala.Core.Relmap.HalfRelmap



-- ----------------------  Implement

{-| Implementation of relmap operator. -}
data Rop c = Rop
    { ropName   :: String     -- ^ Operator name
    , ropGroup  :: String     -- ^ Operator group
    , ropParser :: RopParser  -- ^ Operand parser
    , ropCons   :: RopCons c  -- ^ Constructor of operator
    , ropUsage  :: [String]   -- ^ Usage of operator
    }
    
{-| Parser for operand of relational operator.
    This parsers docompose operand trees,
    and give a name to suboperand. -}
type RopParser
    =  [TokenTree]          -- ^ Unparsed operand
    -> [Named [TokenTree]]  -- ^ Parsed operand

type RopParser'
    =  [Named [TokenTree]]  -- ^ Partially parsed operand
    -> [Named [TokenTree]]  -- ^ Full parsed operand



-- ----------------------  Constructor

{-| Constructor of relational operator 'Relmap'.
    'Relmap' is constructed from 'HalfRelmap' and subrelmaps in it. -}
type RopCons c = RopUse c -> AbortOr (Relmap c)

{-| Use of operator -}
data RopUse c = RopUse {
      ropHalf   :: HalfRelmap   -- ^ Syntactic data of operator use
    , ropSubmap :: [Relmap c]   -- ^ Subrelmaps
    } deriving (Show)




-- ----------------------  Relmap

{-| Retrieve relation from dataset. -}
relmapSource
    :: RopUse c    -- ^ Use of operator
    -> String      -- ^ Operator name
    -> [String]    -- ^ List of term names
    -> (Relmap c)  -- ^ Result relmap
relmapSource use = RelmapSource $ ropHalf use

{-| Constant relmap. -}
relmapConst
    :: RopUse c    -- ^ Use of operator
    -> String      -- ^ Operator name
    -> Rel c       -- ^ Constant relation
    -> Relmap c    -- ^ Result relmap
relmapConst use = RelmapConst $ ropHalf use

{-| Alias relmap. -}
relmapAlias
    :: RopUse c    -- ^ Use of operator
    -> Relmap c    -- ^ 
    -> Relmap c    -- ^ Result relmap
relmapAlias use = RelmapAlias $ ropHalf use

{-| Make a non-confluent relmap. -}
relmapCalc
    :: RopUse c     -- ^ Use of operator
    -> String       -- ^ Operator name
    -> RelmapSub c  -- ^ Calculation of operation
    -> Relmap c     -- ^ Result relmap
relmapCalc use op sub = relmapConfl use op sub []

{-| Make a confluent relmap. -}
relmapConfl
    :: RopUse c     -- ^ Use of operator
    -> String       -- ^ Operator name
    -> RelmapSub c  -- ^ Calculation of operation
    -> [Relmap c]   -- ^ Subrelmaps
    -> Relmap c     -- ^ Result relmap
relmapConfl use = RelmapCalc $ ropHalf use


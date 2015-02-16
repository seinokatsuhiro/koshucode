{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators.

module Koshucode.Baala.Core.Relmap.Rop
  ( -- * Operator
    Rop' (..), RopUsage,
    -- * Constructor
    RopCons', RopUse' (..),
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Lexmap           as C
import qualified Koshucode.Baala.Core.Relmap.Relmap    as C

-- ----------------------  Operator

-- | Implementation of relmap operator.
data Rop' h c = Rop
    { ropName     :: C.RopName        -- ^ Operator name
    , ropGroup    :: String           -- ^ Operator group
    , ropUsage    :: RopUsage         -- ^ Usage text of operator
    , ropAttr     :: C.RopAttr        -- ^ Attribute of operator
    , ropSorter   :: C.AttrSortPara   -- ^ Attribute sorter
    , ropCons     :: RopCons' h c     -- ^ Constructor of operator
    }

instance Show (Rop' h c) where
    show Rop { ropName = name, ropGroup = group }
        = "Rop " ++ group ++ "/" ++ name

instance B.Name (Rop' h c) where
    name = ropName

-- | Usage text of operator.
type RopUsage = String

-- ----------------------  Constructor

-- | Constructor of relmap operator,
--   i.e., function from use of operator to generic relmap.
type RopCons' h c = RopUse' h c -> B.Ab (C.Relmap' h c)

-- | Use of relmap operator.
data RopUse' h c = RopUse
    { ropHook     :: h c
    , ropLexmap   :: C.Lexmap         -- ^ Syntactic data of operator use
    , ropSubmap   :: [C.Relmap' h c]  -- ^ Subrelmaps
    } deriving (Show)

instance B.CodePtr (RopUse' h c) where
    codePtList = B.codePtList . ropLexmap


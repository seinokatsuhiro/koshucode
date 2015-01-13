{-# OPTIONS_GHC -Wall #-}

-- | Implementation of relmap operators.

module Koshucode.Baala.Core.Relmap.Rop
  ( Rop' (..),
    RopUsage,
    RopUse' (..),
    RopCons',
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Lexmap           as C
import qualified Koshucode.Baala.Core.Relmap.Relmap    as C

-- | Implementation of relmap operator
data Rop' h c = Rop
    { ropName     :: C.RopName      -- ^ Operator name
    , ropGroup    :: String         -- ^ Operator group
    , ropSorter   :: C.AttrSort     -- ^ Attribute sorter
    , ropCons     :: RopCons' h c   -- ^ Constructor of operator
    , ropUsage    :: RopUsage       -- ^ Usage of operator
    }

instance Show (Rop' h c) where
    show Rop { ropName = name, ropGroup = group }
        = "Rop " ++ group ++ "/" ++ name

instance B.Name (Rop' h c) where
    name = ropName

-- | Operator usage.
type RopUsage = String

-- | Use of relmap operator
data RopUse' h c = RopUse
    { ropHook       :: h c
    , ropLexmap     :: C.Lexmap         -- ^ Syntactic data of operator use
    , ropSubrelmap  :: [C.Relmap' h c]  -- ^ Subrelmaps
    } deriving (Show)

instance B.CodePtr (RopUse' h c) where
    codePtList = B.codePtList . ropLexmap

-- | Constructor of relmap operator
type RopCons' h c = RopUse' h c -> B.Ab (C.Relmap' h c)


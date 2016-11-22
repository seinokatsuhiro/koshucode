{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators.

module Koshucode.Baala.Core.Relmap.Rop
  ( -- * Operator
    Rop' (..), RopGroup, RopUsage, FindRop',
    -- * Constructor
    RopCons', Intmed' (..),
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Core.Lexmap           as C
import qualified Koshucode.Baala.Core.Relmap.Relmap    as C


-- ----------------------  Operator

-- | Implementation of relmap operator.
data Rop' h c = Rop
    { ropName     :: C.RopName        -- ^ Operator name
    , ropGroup    :: RopGroup         -- ^ Operator group
    , ropUsage    :: RopUsage         -- ^ Usage text of operator
    , ropAttr     :: S.AttrLayout     -- ^ Attribute of operator
    , ropParaze   :: S.AttrParaze     -- ^ Attribute parameterizer
    , ropCons     :: RopCons' h c     -- ^ Constructor of operator
    }

instance Show (Rop' h c) where
    show Rop { ropName = name, ropGroup = group }
        = "Rop " ++ group ++ "/" ++ name

instance B.Name (Rop' h c) where
    name = ropName

-- | Group of operators.
type RopGroup = String

-- | Usage text of operator.
type RopUsage = String

-- | Find relmap operator based on its name.
type FindRop' h c = C.RopName -> Maybe (Rop' h c)


-- ----------------------  Constructor

-- | Constructor of relmap operator,
--   i.e., function from intermediate relamp to generic relmap.
type RopCons' h c = Intmed' h c -> B.Ab (C.Relmap' h c)

-- | Intermediate relmap, that is in between lexmap and generic relmap.
data Intmed' h c = Intmed
    { medHook     :: h c
    , medLexmap   :: C.Lexmap         -- ^ Syntactic data of operator use
    , medSubmap   :: [(String, C.Relmap' h c)]  -- ^ Subrelmaps
    } deriving (Show)

instance B.CodePtr (Intmed' h c) where
    codePtList = B.codePtList . medLexmap


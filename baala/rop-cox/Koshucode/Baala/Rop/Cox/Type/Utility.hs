{-# OPTIONS_GHC -Wall #-}

-- | Utilities for type-related relmap operators.

module Koshucode.Baala.Rop.Cox.Type.Utility
  ( OfParam,
    relkitOfX,
  ) where

import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop

{-| Parameter of @of-x@ type relkit:
    @-let@, @-content@, and optional associations of content. -}
type OfParam c = (K.CopSet c, K.Cox c, [Maybe K.TermName])

{-| Create @of-x@ type relkit. -}
relkitOfX :: (K.CContent c)
  => (K.Ab c -> K.Ab v)   -- ^ Getter of content value
  -> (K.Ab v -> [c])      -- ^ Value to its associations.
  -> OfParam c            -- ^ Relmap parameters
  -> C.RelkitFlow c       -- ^ Relkit
relkitOfX _ _ _ Nothing = C.relkitUnfixed
relkitOfX get contents (cops, cox, ns) (Just he1) = kit where
    ns'    = K.catMaybes ns
    pk     = K.termPicker ns' he1
    he2    = ns' `K.headAppend` he1
    kit    = Rop.newCheck pk $ Right $ C.relkitLineAb False he2 f
    f cs1  = do let c = get $ K.calcCox cops he1 cs1 cox
                    cs2 = K.zipMaybe2 ns $ contents c
                Right $ cs2 ++ cs1

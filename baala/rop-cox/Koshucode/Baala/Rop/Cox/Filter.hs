{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Relation filter.

module Koshucode.Baala.Rop.Cox.Filter
  ( ropsCoxFilter,
  
    -- * keep & omit
    consFilter, relmapFilter, relkitFilter,
  
    -- * contain
    consContain, relmapContain, relkitContain,
  
    -- * omit-all
    consOmitAll, relmapOmitAll,
  ) where

import Prelude hiding (getContents)
import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Base.Message  as Msg


-- | Implementation of relational operators.
ropsCoxFilter :: (K.CContent c) => [C.Rop c]
ropsCoxFilter = Rop.rops "cox-filter"
    [ consContain       K.& [ "contain E"   K.& "-expr" ]
    , consFilter True   K.& [ "keep E"      K.& "-expr* . -let?" ]
    , consFilter False  K.& [ "omit E"      K.& "-expr* . -let?" ]
    , consOmitAll       K.& [ "omit-all"    K.& "" ]
    ]


-- ----------------------  filter

-- | [keep /E/ -let { /D/ | ... }]
--     Keep tuples which expression /E/ is true.
--
--   [omit /E/ -let { /D/ | ... }]
--     Omit tuples which expression /E/ is true.
--
--   @omit@ is the inverse operator of @keep@.
--
--   > keep E = omit not E
--
consFilter :: (K.CContent c) => Bool -> C.RopCons c
consFilter which med =
    do cops   <- Rop.getLet med "-let"
       cox    <- Rop.getCox med "-expr"
       Right $ relmapFilter med (which, cops, cox)

-- | Create @keep@ and @omit@ relmap.
relmapFilter :: (K.CContent c) => C.Intmed c -> (Bool, K.CopSet c, K.Cox c) -> C.Relmap c
relmapFilter med = C.relmapFlow med . relkitFilter

-- | Create @keep@ and @omit@ relkit.
relkitFilter :: (K.CContent c) => (Bool, K.CopSet c, K.Cox c) -> C.RelkitFlow c
relkitFilter _ Nothing = C.relkitUnfixed
relkitFilter (which, cops, cox) (Just he1) = kit where
    kit  = Right $ C.relkitFilterAb he1 test
    test cs1 = do c <- K.calcCox cops he1 cs1 cox
                  case K.isBool c of
                    True  -> Right $ K.gBool c == which
                    False -> Msg.reqBool


-- ----------------------  contain

-- | __contain E__
--
--   Keep tuples in which some terms has content E.
--
consContain :: (K.CContent c) => C.RopCons c
consContain med =
    do c <- Rop.getContent med "-expr"
       Right $ relmapContain med c

-- | Create @contain@ relmap.
relmapContain :: (Eq c) => C.Intmed c -> c -> C.Relmap c
relmapContain med = C.relmapFlow med . relkitContain

-- | Create @contain@ relkit.
relkitContain :: (Eq c) => c -> C.RelkitFlow c
relkitContain _ Nothing = C.relkitUnfixed
relkitContain c (Just he1) = kit where
    kit  = Right $ C.relkitFilterAb he1 test
    test cs1 = Right $ c `elem` cs1


-- ----------------------  omit-all

-- | __omit-all__
--
--   Throw away all tuples.
--
consOmitAll :: C.RopCons c
consOmitAll med = Right $ relmapOmitAll med

-- | Create @omit-all@ relmap.
relmapOmitAll :: C.Intmed c -> C.Relmap c
relmapOmitAll med = C.relmapFlow med relkitOmitAll

-- | Create @omit-all@ relkit.
relkitOmitAll :: C.RelkitFlow c
relkitOmitAll he1 = Right $ C.relkit he1 $ C.RelkitConst []


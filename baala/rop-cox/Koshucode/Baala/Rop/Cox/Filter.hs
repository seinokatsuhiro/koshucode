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
--
--   [@keep E@]
--     Keep tuples @E@ equals true.
-- 
--   [@omit E@]
--     Omit tuples @E@ equals true.
-- 
ropsCoxFilter :: (K.CContent c) => [C.Rop c]
ropsCoxFilter = Rop.rops "cox-filter"
    [ consContain       K.& [ "contain E"   K.& "-expr" ]
    , consFilter True   K.& [ "keep E"      K.& "-in* . -where?" ]
    , consFilter False  K.& [ "omit E"      K.& "-in* . -where?" ]
    , consOmitAll       K.& [ "omit-all"    K.& "" ]
    ]


-- ----------------------  filter

-- | [keep E]
--     Keep tuples which expression E is true.
--
--   [omit E]
--     Omit tuples which expression E is true.
--     @omit E@ is equivalent to @keep not E@.
--
consFilter :: (K.CContent c) => Bool -> C.RopCons c
consFilter b med =
    do cops   <- Rop.getWhere med "-where"
       coxIn  <- Rop.getCox med "-in"
       Right $ relmapFilter med (b, cops, coxIn)

-- | Create @keep@ and @omit@ relmap.
relmapFilter :: (K.CContent c) => C.Intmed c -> (Bool, K.CopSet c, K.Cox c) -> C.Relmap c
relmapFilter med = C.relmapFlow med . relkitFilter

-- | Create @keep@ and @omit@ relkit.
relkitFilter :: (K.CContent c) => (Bool, K.CopSet c, K.Cox c) -> C.RelkitFlow c
relkitFilter _ Nothing = Right C.relkitNothing
relkitFilter (which, cops, body) (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitAbTest p
    p cs1 = do c <- K.coxRunCox cops he1 cs1 body
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
relkitContain _ Nothing = Right C.relkitNothing
relkitContain c (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitAbTest p
    p cs1 = Right $ c `elem` cs1


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


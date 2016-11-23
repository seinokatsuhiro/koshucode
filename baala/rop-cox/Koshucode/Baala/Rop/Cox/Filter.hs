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
import qualified Koshucode.Baala.Data             as D
import qualified Koshucode.Baala.Core             as C
import qualified Koshucode.Baala.Rop.Base         as Rop
import qualified Koshucode.Baala.Rop.Cox.Get      as Rop
import qualified Koshucode.Baala.Rop.Cox.Message  as Msg


-- | Implementation of relational operators.
--
--   [@keep E@]
--     Keep tuples @E@ equals true.
-- 
--   [@omit E@]
--     Omit tuples @E@ equals true.
-- 
ropsCoxFilter :: (D.CContent c) => [C.Rop c]
ropsCoxFilter = Rop.ropList "cox-filter"
    --        CONSTRUCTOR         USAGE         ATTRIBUTE
    [ Rop.def consContain         "contain E"   "-expr"
    , Rop.def (consFilter True)   "keep E"      "-in* . -where?"
    , Rop.def (consFilter False)  "omit E"      "-in* . -where?"
    , Rop.def consOmitAll         "omit-all"    ""
    ]


-- ----------------------  filter

-- | [keep E]
--     Keep tuples which expression E is true.
--
--   [omit E]
--     Omit tuples which expression E is true.
--     @omit E@ is equivalent to @keep not E@.
--
consFilter :: (D.CContent c) => Bool -> C.RopCons c
consFilter b med =
    do cops   <- Rop.getWhere med "-where"
       coxIn  <- Rop.getCox med "-in"
       Right $ relmapFilter med (b, cops, coxIn)

-- | Create @keep@ and @omit@ relmap.
relmapFilter :: (D.CContent c) => C.Intmed c -> (Bool, D.CopSet c, D.Cox c) -> C.Relmap c
relmapFilter med = C.relmapFlow med . relkitFilter

-- | Create @keep@ and @omit@ relkit.
relkitFilter :: (D.CContent c) => (Bool, D.CopSet c, D.Cox c) -> C.RelkitFlow c
relkitFilter _ Nothing = Right C.relkitNothing
relkitFilter (which, cops, body) (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitAbTest p
    p cs1 = do c <- D.coxRunCox cops he1 cs1 body
               case D.isBool c of
                 True  -> Right $ D.gBool c == which
                 False -> Msg.reqBool


-- ----------------------  contain

-- | __contain E__
--
--   Keep tuples in which some terms has content E.
--
consContain :: (D.CContent c) => C.RopCons c
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


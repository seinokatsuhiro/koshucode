{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koshucode.Baala.Op.Cox.Filter
  ( ropsCoxFilter,
  
    -- * keep & omit
    consFilter, relmapFilter, relkitFilter,
  
    -- * contain
    consContain, relmapContain, relkitContain,
  
    -- * omit-all
    consOmitAll, relmapOmitAll,
  ) where

import Prelude hiding (getContents)
import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Data        as C
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Builtin  as Op
import qualified Koshucode.Baala.Op.Cox.Get  as Op
import qualified Koshucode.Baala.Op.Message  as Msg


-- | Implementation of relational operators.
--
--   [@keep E@]
--     Keep tuples @E@ equals true.
-- 
--   [@omit E@]
--     Omit tuples @E@ equals true.
-- 
ropsCoxFilter :: (C.CContent c) => [C.Rop c]
ropsCoxFilter = Op.ropList "cox-filter"
    --        CONSTRUCTOR         USAGE         ATTRIBUTE
    [ Op.def  consContain         "contain E"   "1 -expr"
    , Op.def  (consFilter True)   "keep E"      "V -in | -where"
    , Op.def  (consFilter False)  "omit E"      "V -in | -where"
    , Op.def  consOmitAll         "omit-all"    "0"
    ]


-- ----------------------  filter

consFilter :: (C.CContent c) => Bool -> C.RopCons c
consFilter b med =
    do cops   <- Op.getWhere med "-where"
       coxIn  <- Op.getCox med "-in"
       Right $ relmapFilter med (b, cops, coxIn)

relmapFilter :: (C.CList c, C.CRel c, C.CBool c, B.Write c)
  => C.Intmed c -> (Bool, C.CopSet c, C.Cox c) -> C.Relmap c
relmapFilter med = C.relmapFlow med . relkitFilter

relkitFilter :: (C.CList c, C.CRel c, C.CBool c, B.Write c)
  => (Bool, C.CopSet c, C.Cox c) -> C.RelkitFlow c
relkitFilter _ Nothing = Right C.relkitNothing
relkitFilter (which, cops, body) (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitAbPred p
    p cs1 = do c <- C.coxRunCox cops he1 cs1 body
               case C.isBool c of
                 True  -> Right $ C.gBool c == which
                 False -> Msg.reqBool


-- ----------------------  contain

consContain :: (C.CContent c) => C.RopCons c
consContain med =
    do c <- Op.getContent med "-expr"
       Right $ relmapContain med c

relmapContain :: (Eq c) => C.Intmed c -> c -> C.Relmap c
relmapContain med = C.relmapFlow med . relkitContain

relkitContain :: (Eq c) => c -> C.RelkitFlow c
relkitContain _ Nothing = Right C.relkitNothing
relkitContain c (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitAbPred p
    p cs1 = Right $ c `elem` cs1


-- ----------------------  omit-all

consOmitAll :: C.RopCons c
consOmitAll med = Right $ relmapOmitAll med

relmapOmitAll :: C.Intmed c -> C.Relmap c
relmapOmitAll med = C.relmapFlow med relkitOmitAll

-- | Throw away all tuples in a relation.
relkitOmitAll :: C.RelkitFlow c
relkitOmitAll he1 = Right $ C.relkit he1 $ C.RelkitConst []


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
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Builtin  as Op
import qualified Koshucode.Baala.Op.Cox.Calc as Op
import qualified Koshucode.Baala.Op.Message  as Message


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
    --          CONSTRUCTOR         USAGE         ATTRIBUTE
    [ Op.ropI   consContain         "contain E"   "-expr"
    , Op.ropV   (consFilter True)   "keep E"      "-in | -let"
    , Op.ropV   (consFilter False)  "omit E"      "-in | -let"
    , Op.ropN   consOmitAll         "omit-all"    ""
    ]


-- ----------------------  alpha

ropBase :: C.RopUse c -> [C.Cop c]
ropBase = C.globalFunction . C.ropGlobal

ropBuild :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab (C.Cox c)
ropBuild = C.coxBuild . C.globalSyntax . C.ropGlobal

ropNamedAlphas :: (C.CContent c) => C.RopUse c -> [B.Named B.TokenTree] -> B.Ab [C.NamedCox c]
ropNamedAlphas use = mapM (B.namedMapM $ ropBuild use)

getNamedCoxes :: (C.CContent c) => C.RopUse c -> String -> B.Ab [C.NamedCox c]
getNamedCoxes use = ropNamedAlphas use B.<=< Op.getWordTrees use 


-- ----------------------  filter

consFilter :: (C.CContent c) => Bool -> C.RopCons c
consFilter b use =
    do coxLet  <- Op.getOption [] getNamedCoxes use "-let"
       coxIn   <- Op.getCox use "-in"
       Right $ relmapFilter use (b, (ropBase use, coxLet), coxIn)

relmapFilter :: (C.CList c, C.CRel c, C.CBool c, B.Write c)
  => C.RopUse c -> (Bool, C.CopBundle c, C.Cox c) -> C.Relmap c
relmapFilter use = C.relmapFlow use . relkitFilter

relkitFilter :: (C.CList c, C.CRel c, C.CBool c, B.Write c)
  => (Bool, C.CopBundle c, C.Cox c) -> C.RelkitFlow c
relkitFilter _ Nothing = Right C.relkitNothing
relkitFilter (which, cops, body) (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitAbPred p
    p cs1 = do c <- C.coxRunCox cops he1 cs1 body
               case C.isBool c of
                 True  -> Right $ C.gBool c == which
                 False -> Message.reqBool


-- ----------------------  contain

consContain :: (C.CContent c) => C.RopCons c
consContain use =
    do c <- Op.getContent use "-expr"
       Right $ relmapContain use c

relmapContain :: (Eq c) => C.RopUse c -> c -> C.Relmap c
relmapContain use = C.relmapFlow use . relkitContain

relkitContain :: (Eq c) => c -> C.RelkitFlow c
relkitContain _ Nothing = Right C.relkitNothing
relkitContain c (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitAbPred p
    p cs1 = Right $ c `elem` cs1


-- ----------------------  omit-all

consOmitAll :: C.RopCons c
consOmitAll use = Right $ relmapOmitAll use

relmapOmitAll :: C.RopUse c -> C.Relmap c
relmapOmitAll use = C.relmapFlow use relkitOmitAll

-- | Throw away all tuples in a relation.
relkitOmitAll :: C.RelkitFlow c
relkitOmitAll he1 = Right $ C.relkit he1 $ C.RelkitConst []


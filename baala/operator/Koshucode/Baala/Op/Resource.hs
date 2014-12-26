{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Resource
  ( ropsResource,
    -- * koshu-res-rop
    consKoshuResRop, relkitKoshuResRop,
  ) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Op.Builtin    as Op


-- | Implementation of relational operators.
--
--   [@koshu-cop \/N@]
--     Retrieve list of content operators.
-- 
--   [@koshu-cop-infix \/N \[ -height \/N \]\[ -dir \/N \]@]
--     Retrieve list of infix specifications.
-- 
--   [@koshu-rop /N@]
--     Retrieve list of relmap operators.
-- 
--   [@koshu-version /N@]
--     Get version number of the koshu calculator.
-- 
ropsResource :: (C.CContent c) => [C.Rop c]
ropsResource = Op.ropList "resource"
    --          CONSTRUCTOR        USAGE                 ATTRIBUTE
    [ Op.ropII  consKoshuResRop    "koshu-res-rop /N /N" "-sec -name"
    ]


-- ----------------------  koshu-res-rop

consKoshuResRop :: (C.CContent c) => C.RopCons c
consKoshuResRop use =
  do sec   <- Op.getTerm use "-sec"
     name  <- Op.getTerm use "-name"
     Right $ relmapKoshuResRop use (sec, name)

relmapKoshuResRop :: (C.CContent c)
    => C.RopUse c -> (B.TermName, B.TermName)
    -> C.Relmap c
relmapKoshuResRop use = C.relmapHook use . relkitKoshuResRop

relkitKoshuResRop :: (C.CContent c)
    => (B.TermName, B.TermName)
    -> C.RelkitHook c
relkitKoshuResRop (sec, name) res _ = Right kit2 where
    kit2  = C.relkitConstBody ns bo2
    ns    = [sec, name]
    bo2   = map f $ C.resRelmap res
    f ((s, n), _) = [C.pDecFromInt s, C.pText n]



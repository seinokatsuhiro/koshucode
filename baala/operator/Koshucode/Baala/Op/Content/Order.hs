{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Content.Order
( copsOrder
  -- $Operators
) where

import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Message as Abort

-- ----------------------
-- $Operators
--
--  [@=@]     Equal.
--
--  [@\<\>@]  Not equal.
--
--  [@<@]     Less than.
--
--  [@<=@]    Less than or equal.
--
--  [@>@]     Grater than.
--
--  [@>=@]    Grater than or equal.
--

copsOrder :: (C.CBool c, Eq c, Ord c) => [C.Cop c]
copsOrder =
    [ C.CopFun  "="    copEq
    , C.CopFun  "<>"   copNeq
    , C.CopFun  "<"    copLt
    , C.CopFun  "<="   copLte
    , C.CopFun  ">"    copGt
    , C.CopFun  ">="   copGte
    ]

copBy :: (C.CBool c) => (c -> c -> Bool) -> C.CopFun c
copBy p [Right x, Right y] = C.putBool $ x `p` y
copBy _ _  = Abort.notFound ""

copEq   :: (C.CBool c, Eq c) => C.CopFun c
copEq   =  copBy (==)

copNeq  :: (C.CBool c, Eq c) => C.CopFun c
copNeq  =  copBy (/=)

copLt   :: (C.CBool c, Ord c) => C.CopFun c
copLt   =  copBy (<)

copLte  :: (C.CBool c, Ord c) => C.CopFun c
copLte  =  copBy (<=)

copGt   :: (C.CBool c, Ord c) => C.CopFun c
copGt   =  copBy (>)

copGte  :: (C.CBool c, Ord c) => C.CopFun c
copGte  =  copBy (>=)


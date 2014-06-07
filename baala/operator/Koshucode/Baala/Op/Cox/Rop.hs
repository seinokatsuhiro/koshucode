{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Cox.Rop
( coxRops,
  -- $Operators
) where

import qualified Koshucode.Baala.Core           as C
import qualified Koshucode.Baala.Op.Builtin     as Op
import qualified Koshucode.Baala.Op.Cox.Cox     as Op

-- | Implementation of relational operators.
coxRops :: (C.CContent c) => [C.Rop c]
coxRops = Op.ropList "cox"
    --  SYNOPSIS,
    --  CONSTRUCTOR, ATTRIBUTE
    [ ( "add /N E ...",
        Op.consAdd, C.roaList "-in" ["-let"] )
    , ( "keep E",
        Op.consFilter True, C.roaList "-in" ["-let"] )
    , ( "omit E",
        Op.consFilter False, C.roaList "-in" ["-let"] )
    , ( "range /N -from E -to E",
        Op.consRange, C.roaOne "-term" ["-from", "-to"] )
    , ( "split /N E ...",
        Op.consSplit, C.roaList "-in" ["-let"] )
    , ( "subst /N E ...",
        Op.consSubst, C.roaList "-in" ["-let"] )
    ]

-- ----------------------
-- $Operators
--
--  [@add \/N E ...@]
--    Add terms of name @\/N@ and content @E@ ...
--
--  [@keep E@]
--    Keep tuples @E@ equals true.
-- 
--  [@omit E@]
--    Omit tuples @E@ equals true.
-- 


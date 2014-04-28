{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Meta.Rop
( metaRops,
  -- $Operators
) where

import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Op.Builtin      as Op
import qualified Koshucode.Baala.Op.Meta.Meta    as Op

-- | Implementation of relational operators.
metaRops :: (C.CContent c) => [C.Rop c]
metaRops = Op.ropList "meta"
    --  SYNOPSIS,
    --  CONSTRUCTOR, OPERAND
    [ ( "koshu-cop /N",
        Op.consKoshuCop, C.rodList "-name" [] )
    , ( "koshu-cop-infix /N [ -height /N ][ -dir /N ]",
        Op.consKoshuCopInfix, C.rodOne "-name" ["-height", "-dir"] )
    , ( "koshu-rop /N /N",
        Op.consKoshuRop, C.rodList "-name" ["-group", "-usage"] )
    , ( "koshu-version /N",
        Op.consKoshuVersion, C.rodOneList "-term" "-version" [] )
    ]

-- ----------------------
-- $Operators
--
--  [@koshu-cop \/N@]
--    Retrieve list of content operators.
-- 
--  [@koshu-cop-infix \/N \[ -height \/N \]\[ -dir \/N \]@]
--    Retrieve list of infix specifications.
-- 
--  [@koshu-rop /N@]
--    Retrieve list of relmap operators.
-- 
--  [@koshu-version /N@]
--    Get version number of the koshu calculator.
-- 


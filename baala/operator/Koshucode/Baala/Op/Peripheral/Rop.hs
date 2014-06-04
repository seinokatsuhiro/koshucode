{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Peripheral.Rop
( peripheralRops,
  -- $Operators
) where

import qualified Koshucode.Baala.Core                  as C
import qualified Koshucode.Baala.Op.Builtin            as Op
import qualified Koshucode.Baala.Op.Peripheral.Flow    as Op

-- | Implementation of relational operators.
peripheralRops :: (C.CContent c) => [C.Rop c]
peripheralRops = Op.ropList "peripheral"
    --  SYNOPSIS,
    --  CONSTRUCTOR, ATTRIBUTE
    [ ( "assn /P ... -to N.",
        Op.consAssn, C.roaList "-term" ["-to"] )
    , ( "member /N /N",
        Op.consMember, C.roaEnum ["-1", "-2"] [] )
    , ( "rdf P /S /O",
        Op.consRdf, C.roaOneList "-pattern" "-term" [] )
    , ( "typename /N /P ...",
        Op.consTypename, C.roaList "-term" [] )
    , ( "unassn /P -only /P ...",
        Op.consUnassn, C.roaOne "-from" ["-only"] )
    ]

-- ----------------------
-- $Operators
--
--  [@member \/N \/N@]
--    Membership of set or list.
-- 
--  [@rdf P \/S \/O@]
--    Retrieve relation from RDF-like judgements.
-- 

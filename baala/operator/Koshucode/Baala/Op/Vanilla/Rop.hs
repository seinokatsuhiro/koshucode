{-# OPTIONS_GHC -Wall #-}

-- | Vanilla relational operators.

module Koshucode.Baala.Op.Vanilla.Rop
( vanillaRops,
  -- $Operators
) where

import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Op.Builtin         as Op
import qualified Koshucode.Baala.Op.Vanilla.Check   as Op
import qualified Koshucode.Baala.Op.Vanilla.Confl   as Op
import qualified Koshucode.Baala.Op.Vanilla.Cox     as Op
import qualified Koshucode.Baala.Op.Vanilla.Flow    as Op
import qualified Koshucode.Baala.Op.Vanilla.Naming  as Op
import qualified Koshucode.Baala.Op.Vanilla.Order   as Op

-- | Implementation of relational operators.
vanillaRops :: (C.CContent c) => [C.Rop c]
vanillaRops = Op.ropList "vanilla"
    --  SYNOPSIS,
    --  CONSTRUCTOR, OPERAND
    [ ( "add /N E ...",
        Op.consAdd, C.rodList "-in" ["-let"] )
    , ( "check-term [ -just /N ... | -has /N ... | -but /N ... ]",
        Op.consCheckTerm, C.rodNone ["-just", "-has", "-but"] )
    , ( "compose R",
        Op.consCompose, C.rodOne "-relmap" [] )
    , ( "const R",
        Op.consConst, C.rodOne "-lit" [] )
    , ( "dump",
        Op.consDump, C.rodNone [] )
    , ( "duplicate /N ...",
        Op.consDuplicate, C.rodList "-term" [] )
    , ( "fix R",
        Op.consFix, C.rodOne "-relmap" [] )
    , ( "fix-join R",
        Op.consFixJoin, C.rodOne "-relmap" [] )
    , ( "both R",
        Op.consBoth, C.rodOne "-relmap" [] )
    , ( "if R ...",
        Op.consIf, C.rodList "-relmap" [] )
    , ( "keep E",
        Op.consFilter True, C.rodList "-in" ["-let"] )
    , ( "maybe R",
        Op.consMaybe, C.rodOne "-relmap" [] )
    , ( "member /N /N",
        Op.consMember, C.rodEnum ["-1", "-2"] [] )
    , ( "number /N -order /N ...",
        Op.consNumber, C.rodOne "-term" ["-order", "-from"] )
    , ( "omit E",
        Op.consFilter False, C.rodList "-in" ["-let"] )
    , ( "prefix /P /N ...",
        Op.consPrefix, C.rodOneList "-prefix" "-term" [] )
    , ( "prefix-change /P /Q",
        Op.consPrefixChange, C.rodTwo "-new" "-old" [] )
    , ( "range /N -from E -to E",
        Op.consRange, C.rodOne "-term" ["-from", "-to"] )
    , ( "rank /N -order /N ...",
        Op.consRank, C.rodOne "-term" ["-order", "-dense"] )
    , ( "rdf P /S /O",
        Op.consRdf, C.rodOneList "-pattern" "-term" [] )
    , ( "repeat N R",
        Op.consRepeat, C.rodTwo "-count" "-relmap" [] )
    , ( "size /N",
        Op.consSize, C.rodOne "-term" [] )
    , ( "subst /N E ...",
        Op.consSubst, C.rodList "-in" ["-let"] )
    , ( "typename /N /P ...",
        Op.consTypename, C.rodList "-term" [] )
    , ( "unless R R",
        Op.consUnless, C.rodList "-relmap" [] )
    , ( "unprefix /P",
        Op.consUnprefix, C.rodOne "-prefix" [] )
    , ( "when R R",
        Op.consWhen, C.rodList "-relmap" [] )
    ]

-- ----------------------
-- $Operators
--
--  [@add \/N E ...@]
--    Add terms of name @\/N@ and content @E@ ...
--
--  [@check-term \[ -just \/P ... | -has \/P ... | -but \/N ... \]@]
--    Check occurences of terms for input relation.
--
--  [@duplicate \/P ...@]
--    Pass duplicate tuples on @\/P@ ...
--
--  [@keep E@]
--    Keep tuples @E@ equals true.
-- 
--  [@maybe R@]
--    Meet input and given relation.
--    It keeps input tuples of which counterparts are totally negated.
-- 
--  [@member \/N \/N@]
--    Membership of set or list.
-- 
--  [@number \/N \[ -order \/P ... \]@]
--    Add numbering term @\/N@ ordered by @\/P@ ...
--
--  [@omit E@]
--    Omit tuples @E@ equals true.
-- 
--  [@prefix \/P \/N ...@]
--    Add prefix @\/P@ to terms @\/N@ ...
-- 
--  [@prefix-change \/P \/Q@]
--    Change prefix from @\/P@ to @\/Q@.
-- 
--  [@rank \/N -order \/P ... \[ -dense \]@]
--    Add term @\/N@ for ranking ordered by @\/P@ ...
--
--  [@rdf P \/S \/O@]
--    Retrieve relation from RDF-like judgements.
-- 
--  [@size \/N@]
--    Calculate cardinality of input relation.
-- 
--  [@unprefix \/P@]
--    Remove prefix @\/P@ from term name.

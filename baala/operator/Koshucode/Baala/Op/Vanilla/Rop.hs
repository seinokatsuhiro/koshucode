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
    --  CONSTRUCTOR, ATTRIBUTE
    [ ( "add /N E ...",
        Op.consAdd, C.roaList "-in" ["-let"] )
    , ( "check-term [ -just /N ... | -has /N ... | -but /N ... ]",
        Op.consCheckTerm, C.roaNone ["-just", "-has", "-but"] )
    , ( "chunk /T ... [-order /P ...]",
        Op.consChunk, C.roaList "-term" ["-order"] )
    , ( "compose R",
        Op.consCompose, C.roaOne "-relmap" [] )
    , ( "const R",
        Op.consConst, C.roaOne "-lit" [] )
    , ( "dump",
        Op.consDump, C.roaNone [] )
    , ( "duplicate /N ...",
        Op.consDuplicate, C.roaList "-term" [] )
    , ( "fix R",
        Op.consFix, C.roaOne "-relmap" [] )
    , ( "fix-join R",
        Op.consFixJoin, C.roaOne "-relmap" [] )
    , ( "both R",
        Op.consBoth, C.roaOne "-relmap" [] )
    , ( "if R ...",
        Op.consIf, C.roaList "-relmap" [] )
    , ( "keep E",
        Op.consFilter True, C.roaList "-in" ["-let"] )
    , ( "maybe R",
        Op.consMaybe, C.roaOne "-relmap" [] )
    , ( "member /N /N",
        Op.consMember, C.roaEnum ["-1", "-2"] [] )
    , ( "number /N -order /N ...",
        Op.consNumber, C.roaOne "-term" ["-order", "-from"] )
    , ( "omit E",
        Op.consFilter False, C.roaList "-in" ["-let"] )
    , ( "prefix /P /N ...",
        Op.consPrefix, C.roaOneList "-prefix" "-term" [] )
    , ( "prefix-change /P /Q",
        Op.consPrefixChange, C.roaTwo "-new" "-old" [] )
    , ( "range /N -from E -to E",
        Op.consRange, C.roaOne "-term" ["-from", "-to"] )
    , ( "rank /N -order /N ...",
        Op.consRank, C.roaOne "-term" ["-order", "-dense"] )
    , ( "rdf P /S /O",
        Op.consRdf, C.roaOneList "-pattern" "-term" [] )
    , ( "repeat N R",
        Op.consRepeat, C.roaTwo "-count" "-relmap" [] )
    , ( "size /N",
        Op.consSize, C.roaOne "-term" [] )
    , ( "subst /N E ...",
        Op.consSubst, C.roaList "-in" ["-let"] )
    , ( "typename /N /P ...",
        Op.consTypename, C.roaList "-term" [] )
    , ( "unless R R",
        Op.consUnless, C.roaList "-relmap" [] )
    , ( "unprefix /P",
        Op.consUnprefix, C.roaOne "-prefix" [] )
    , ( "when R R",
        Op.consWhen, C.roaList "-relmap" [] )
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

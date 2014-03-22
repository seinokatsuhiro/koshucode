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
import qualified Koshucode.Baala.Op.Vanilla.Global  as Op
import qualified Koshucode.Baala.Op.Vanilla.Naming  as Op
import qualified Koshucode.Baala.Op.Vanilla.Order   as Op

-- | Implementation of relational operators.
vanillaRops :: (C.CContent c) => [C.Rop c]
vanillaRops = Op.ropList "vanilla"
    --  SYNOPSIS,
    --  CONSTRUCTOR, OPERAND
    [ ( "add /N E ...",
        Op.consAdd, C.sortList "-in" ["-let"] )
    , ( "check-term [ -just /N ... | -has /N ... | -but /N ... ]",
        Op.consCheckTerm, C.sortNone ["-just", "-has", "-but"] )
    , ( "compose R",
        Op.consCompose, C.sortOne "-relmap" [] )
    , ( "do R ...",
        Op.consDo, C.sortList "-relmap" ["-let"] )
    , ( "duplicate /N ...",
        Op.consDuplicate, C.sortList "-term" [] )
    , ( "enclose /N",
        Op.consEnclose, C.sortOne "-term" [] )
    , ( "fix R",
        Op.consFix, C.sortOne "-relmap" [] )
    , ( "fix-join R",
        Op.consFixJoin, C.sortOne "-relmap" [] )
    , ( "full R ...",
        Op.consFull, C.sortList "-relmap" [] )
    , ( "group /N R",
        Op.consGroup, C.sortTwo "-term" "-relmap" [] )
    , ( "hold E",
        Op.consFilter True, C.sortList "-in" ["-let"] )
    , ( "if R ...",
        Op.consIf, C.sortList "-relmap" [] )
    , ( "keep E",
        Op.consFilter True, C.sortList "-in" ["-let"] )
    , ( "koshu-cop /N",
        Op.consKoshuCop, C.sortList "-name" [] )
    , ( "koshu-cop-infix /N [ -height /N ][ -dir /N ]",
        Op.consKoshuCopInfix, C.sortOne "-name" ["-height", "-dir"] )
    , ( "koshu-rop /N",
        Op.consKoshuRop, C.sortList "-name" [] )
    , ( "koshu-version /N",
        Op.consKoshuVersion, C.sortOneList "-term" "-version" [] )
    , ( "maybe R",
        Op.consMaybe, C.sortOne "-relmap" [] )
    , ( "member /N /N",
        Op.consMember, C.sortEnum ["-1", "-2"] [] )
    , ( "number /N -order /N ...",
        Op.consNumber, C.sortOne "-term" ["-order"] )
    , ( "omit E",
        Op.consFilter False, C.sortList "-in" ["-let"] )
    , ( "prefix /P /N ...",
        Op.consPrefix, C.sortOneList "-prefix" "-term" [] )
    , ( "prefix-change /P /Q",
        Op.consPrefixChange, C.sortTwo "-new" "-old" [] )
    , ( "range /N -from E -to E",
        Op.consRange, C.sortOne "-term" ["-from", "-to"] )
    , ( "rank /N -order /N ...",
        Op.consRank, C.sortOne "-term" ["-order", "-dense"] )
    , ( "rdf P /S /O",
        Op.consRdf, C.sortOneList "-pattern" "-term" [] )
    , ( "repeat N R",
        Op.consRepeat, C.sortTwo "-count" "-relmap" [] )
    , ( "size /N",
        Op.consSize, C.sortOne "-term" [] )
    , ( "subst /N E ...",
        Op.consSubst, C.sortList "-in" ["-let"] )
    , ( "typename /N /P ...",
        Op.consTypename, C.sortList "-term" [] )
    , ( "unless R R",
        Op.consUnless, C.sortList "-relmap" [] )
    , ( "unprefix /P",
        Op.consUnprefix, C.sortOne "-prefix" [] )
    , ( "when R R",
        Op.consWhen, C.sortList "-relmap" [] )
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
--  [@enclose \/N@]
--    Enclose input relation in a term.
--
--  [@group \/N R@]
--    Group tuples in @R@ by input relation.
--
--  [@keep E@]
--    Keep tuples @E@ equals true.
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

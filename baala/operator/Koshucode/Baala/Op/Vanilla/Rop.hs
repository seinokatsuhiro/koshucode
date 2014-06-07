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
import qualified Koshucode.Baala.Op.Vanilla.Flow    as Op
import qualified Koshucode.Baala.Op.Vanilla.Naming  as Op

-- | Implementation of relational operators.
vanillaRops :: (C.CContent c) => [C.Rop c]
vanillaRops = Op.ropList "vanilla"
    --  SYNOPSIS,
    --  CONSTRUCTOR, ATTRIBUTE
    [ ( "check-term [ -just /N ... | -has /N ... | -but /N ... ]",
        Op.consCheckTerm, C.roaNone ["-just", "-has", "-but"] )
    , ( "compose R",
        Op.consCompose, C.roaOne "-relmap" [] )
    , ( "dump",
        Op.consDump, C.roaNone [] )
    , ( "duplicate /N ...",
        Op.consDuplicate, C.roaList "-term" [] )
    , ( "both R",
        Op.consBoth, C.roaOne "-relmap" [] )
    , ( "maybe R",
        Op.consMaybe, C.roaOne "-relmap" [] )
    , ( "number /N -order /N ...",
        Op.consNumber, C.roaOne "-term" ["-order", "-from"] )
    , ( "prefix /P /N ...",
        Op.consPrefix, C.roaOneList "-prefix" "-term" [] )
    , ( "prefix-change /P /Q",
        Op.consPrefixChange, C.roaTwo "-new" "-old" [] )
    , ( "rank /N -order /N ...",
        Op.consRank, C.roaOne "-term" ["-order", "-dense"] )
    , ( "size /N",
        Op.consSize, C.roaOne "-term" [] )
    , ( "unprefix /P",
        Op.consUnprefix, C.roaOne "-prefix" [] )
    ]

-- ----------------------
-- $Operators
--
--  [@check-term \[ -just \/P ... | -has \/P ... | -but \/N ... \]@]
--    Check occurences of terms for input relation.
--
--  [@duplicate \/P ...@]
--    Pass duplicate tuples on @\/P@ ...
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

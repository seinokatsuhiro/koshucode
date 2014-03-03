{-# OPTIONS_GHC -Wall #-}

{-| Vanilla relational operators. -}

module Koshucode.Baala.Vanilla.Rop.Rops
( vanillaRops,
  -- $Operators
) where

import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop

import qualified Koshucode.Baala.Vanilla.Rop.Calc    as Rop
import qualified Koshucode.Baala.Vanilla.Rop.Check   as Rop
import qualified Koshucode.Baala.Vanilla.Rop.Confl   as Rop
import qualified Koshucode.Baala.Vanilla.Rop.Cox     as Rop
import qualified Koshucode.Baala.Vanilla.Rop.Global  as Rop
import qualified Koshucode.Baala.Vanilla.Rop.Order   as Rop
import qualified Koshucode.Baala.Vanilla.Rop.Naming  as Rop
import qualified Koshucode.Baala.Vanilla.Type        as Rop

{-| Implementation of relational operators. -}
vanillaRops :: [C.Rop Rop.VContent]
vanillaRops = Rop.ropList "vanilla"
    --  SYNOPSIS,
    --  CONSTRUCTOR, OPERAND
    [ ( "add /N E ...",
        Rop.consAdd, C.sortList "-in" ["-let"] )
    , ( "check-term [ -just /N ... | -has /N ... | -but /N ... ]",
        Rop.consCheckTerm, C.sortNone ["-just", "-has", "-but"] )
    , ( "do R ...",
        Rop.consDo, C.sortList "-relmap" [] )
    , ( "duplicate /N ...",
        Rop.consDuplicate, C.sortList "-term" [] )
    , ( "enclose /N",
        Rop.consEnclose, C.sortOne "-term" [] )
    , ( "full R ...",
        Rop.consFull, C.sortOne "-relmap" [] )
    , ( "group /N R",
        Rop.consGroup, C.sortTwo "-term" "-relmap" [] )
    , ( "hold E",
        Rop.consFilter True, C.sortList "-in" ["-let"] )
    , ( "if R ...",
        Rop.consIf, C.sortList "-relmap" [] )
    , ( "keep E",
        Rop.consFilter True, C.sortList "-in" ["-let"] )
    , ( "koshu-cop /N",
        Rop.consKoshuCop, C.sortList "-name" [] )
    , ( "koshu-cop-infix /N [ -height /N ][ -dir /N ]",
        Rop.consKoshuCopInfix, C.sortOne "-name" ["-height", "-dir"] )
    , ( "koshu-rop /N",
        Rop.consKoshuRop, C.sortList "-name" [] )
    , ( "koshu-version /N",
        Rop.consKoshuVersion, C.sortOneList "-term" "-version" [] )
    , ( "maybe R",
        Rop.consMaybe, C.sortOne "-relmap" [] )
    , ( "member /N /N",
        Rop.consMember, C.sortEnum ["-1", "-2"] [] )
    , ( "number /N -order /N ...",
        Rop.consNumber, C.sortOne "-term" ["-order"] )
    , ( "omit E",
        Rop.consFilter False, C.sortList "-in" ["-let"] )
    , ( "prefix /P /N ...",
        Rop.consPrefix, C.sortOneList "-prefix" "-term" [] )
    , ( "prefix-change /P /Q",
        Rop.consPrefixChange, C.sortTwo "-new" "-old" [] )
    , ( "rank /N -order /N ...",
        Rop.consRank, C.sortOne "-term" ["-order", "-dense"] )
    , ( "range /N -from E -to E",
        Rop.consRange, C.sortOne "-term" ["-from", "-to"] )
    , ( "rdf P /S /O",
        Rop.consRdf, C.sortOneList "-pattern" "-term" [] )
    , ( "size /N",
        Rop.consSize, C.sortOne "-term" [] )
    , ( "typename /N /P ...",
        Rop.consTypename, C.sortList "-term" [] )
    , ( "unprefix /P",
        Rop.consUnprefix, C.sortOne "-prefix" [] )
    ]

-- ----------------------
{- $Operators

   [@add \/N E ...@]
     Add terms of name @\/N@ and content @E@ ...

   [@check-term \[ -just \/P ... | -has \/P ... | -but \/N ... \]@]
     Check occurences of terms for input relation.

   [@duplicate \/P ...@]
     Pass duplicate tuples on @\/P@ ...

   [@enclose \/N@]
     Enclose input relation in a term.

   [@group \/N R@]
     Group tuples in @R@ by input relation.

   [@keep E@]
     Keep tuples @E@ equals true.
  
   [@koshu-cop \/N@]
     Retrieve list of content operators.
  
   [@koshu-cop-infix \/N \[ -height \/N \]\[ -dir \/N \]@]
     Retrieve list of infix specifications.
  
   [@koshu-rop /N@]
     Retrieve list of relmap operators.
  
   [@koshu-version /N@]
     Get version number of the koshu calculator.
  
   [@maybe R@]
     Meet input and given relation.
     It keeps input tuples of which counterparts are totally negated.
  
   [@member \/N \/N@]
     Membership of set or list.
  
   [@number \/N \[ -order \/P ... \]@]
     Add numbering term @\/N@ ordered by @\/P@ ...

   [@omit E@]
     Omit tuples @E@ equals true.
  
   [@prefix \/P \/N ...@]
     Add prefix @\/P@ to terms @\/N@ ...
  
   [@prefix-change \/P \/Q@]
     Change prefix from @\/P@ to @\/Q@.
  
   [@rank \/N -order \/P ... \[ -dense \]@]
     Add term @\/N@ for ranking ordered by @\/P@ ...

   [@rdf P \/S \/O@]
     Retrieve relation from RDF-like judgements.
  
   [@size \/N@]
     Calculate cardinality of input relation.
  
   [@unprefix \/P@]
     Remove prefix @\/P@ from term name.

-}


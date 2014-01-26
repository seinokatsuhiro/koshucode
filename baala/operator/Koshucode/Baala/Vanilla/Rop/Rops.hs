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
        Rop.ropConsAdd, C.operandList "-term" [] )
    , ( "check-term [ -just /N ... | -has /N ... | -but /N ... ]",
        Rop.ropConsCheckTerm, C.operandNone ["-just", "-has", "-but"] )
    , ( "duplicate /N ...",
        Rop.ropConsDuplicate, C.operandList "-term" [] )
    , ( "enclose /N",
        Rop.ropConsEnclose, C.operandOne "-term" [] )
    , ( "full R ...",
        Rop.ropConsFull, C.operandOne "-relmap" [] )
    , ( "group /N R",
        Rop.ropConsGroup, C.operandTwo "-term" "-relmap" [] )
    , ( "hold E",
        Rop.ropConsHold, C.operandList "-term" [] )
    , ( "koshu-cop /N",
        Rop.ropConsKoshuCop, C.operandList "-name" [] )
    , ( "koshu-cop-infix /N [ -height /N ][ -dir /N ]",
        Rop.ropConsKoshuCopInfix, C.operandOne "-name" ["-height", "-dir"] )
    , ( "koshu-rop /N",
        Rop.ropConsKoshuRop, C.operandList "-name" [] )
    , ( "maybe R",
        Rop.ropConsMaybe, C.operandOne "-relmap" [] )
    , ( "member /N /N",
        Rop.ropConsMember, C.operandEnum ["-1", "-2"] [] )
    , ( "number /N -order /N ...",
        Rop.ropConsNumber, C.operandOne "-term" ["-order"] )
    , ( "prefix /P /N ...",
        Rop.ropConsPrefix, C.operandOneList "-prefix" "-term" [] )
    , ( "prefix-change /P /Q",
        Rop.ropConsPrefixChange, C.operandTwo "-new" "-old" [] )
    , ( "rank /N -order /N ...",
        Rop.ropConsRank, C.operandOne "-term" ["-order", "-dense"] )
    , ( "range /N -from E -to E",
        Rop.ropConsRange, C.operandOne "-term" ["-from", "-to"] )
    , ( "rdf P /S /O",
        Rop.ropConsRdf, C.operandOneList "-pattern" "-term" [] )
    , ( "size /N",
        Rop.ropConsSize, C.operandOne "-term" [] )
    , ( "typename /N /P ...",
        Rop.ropConsTypename, C.operandList "-term" [] )
    , ( "unprefix /P",
        Rop.ropConsUnprefix, C.operandOne "-prefix" [] )
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

   [@hold E@]
     Keep tuples @E@ equals true.
  
   [@koshu-cop \/N@]
     Retrieve list of content operators.
  
   [@koshu-cop-infix \/N \[ -height \/N \]\[ -dir \/N \]@]
     Retrieve list of infix specifications.
  
   [@koshu-rop /N@]
     Retrieve list of relmap operators.
  
   [@maybe R@]
     Meet input and given relation.
     It keeps input tuples of which counterparts are totally negated.
  
   [@number \/N \[ -order \/P ... \]@]
     Add numbering term @\/N@ ordered by @\/P@ ...

   [@member \/N \/N@]
     Membership of set or list.
  
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


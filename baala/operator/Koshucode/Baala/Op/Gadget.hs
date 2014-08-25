{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Gadget
( ropsGadget,

  -- * contents
  consContents, relmapContents,

  -- * size
  consSize, relmapSize, relkitSize,
  -- $size
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Message as Message


-- | Gadgets
--
--   [@contents@]
--     Make nary relation of all contetnts.
--
--   [@number \/N \[ -order \/P ... \]@]
--     Add numbering term @\/N@ ordered by @\/P@ ...
-- 
--   [@rank \/N -order \/P ... \[ -dense \]@]
--     Add term @\/N@ for ranking ordered by @\/P@ ...
-- 
--   [@size \/N@]
--     Calculate cardinality of input relation.
--
ropsGadget :: (C.CContent c) => [C.Rop c]
ropsGadget = Op.ropList "gadget"  -- GROUP
    --         CONSTRUCTOR  USAGE                      ATTRIBUTE
    [ Op.ropV consContents  "contents /N"              "-term"
    , Op.ropV consDumpTree  "dump-tree X"              "-tree"
    , Op.ropI consSize      "size /N"                  "-term"
    ]


-- ----------------------  contents

consContents :: (Ord c) => C.RopCons c
consContents use =
    do n <- Op.getTerm use "-term"
       Right $ relmapContents use n

relmapContents :: (Ord c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapContents use = C.relmapFlow use . relkitContents

relkitContents :: (Ord c) => B.TermName -> C.RelkitFlow c
relkitContents n _ = Right $ C.relkitJust he2 $ C.RelkitFull False kitf where
    he2  = B.headFrom [n]
    kitf = map B.li1 . B.unique . concat


-- ----------------------  size

-- $size
--
--  Count number of tuples in the output of relmap @a@.
--  
--    > a | size /c
--

consSize :: (C.CDec c) => C.RopCons c
consSize use =
  do n <- Op.getTerm use "-term"
     Right $ relmapSize use n

relmapSize :: (C.CDec c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapSize use n = C.relmapFlow use $ relkitSize n

relkitSize :: (C.CDec c) => B.TermName -> C.RelkitFlow c
relkitSize n _ = Right kit2 where
    he2       = B.headFrom [n]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = [[ C.pDecFromInt $ length bo1 ]]


-- ----------------------  dump-tree

consDumpTree :: (C.CDec c) => C.RopCons c
consDumpTree use =
  do trees <- Op.getTrees use "-tree"
     Message.dumpTrees trees


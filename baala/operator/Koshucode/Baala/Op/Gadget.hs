{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Gadget
  ( ropsGadget,
  
    -- * contents
    consContents, relmapContents,
  
    -- * dependent-rank
    consDepRank, relmapDepRank,
  
    -- * size
    consSize, relmapSize, relkitSize,
    -- $size
  
    -- * dump-tree
    consDumpTree,
  ) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.DepRank as Op
import qualified Koshucode.Baala.Op.Message as Msg


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
    --        CONSTRUCTOR   USAGE                      ATTRIBUTE
    [ Op.def  consContents  "contents /N"              "V -term"
    , Op.def  consDumpTree  "dump-tree X"              "V -tree"
    , Op.def  consDepRank   "dependent-rank /P /P -rank /N"  "2 -x -y | -rank"
    , Op.def  consSize      "size /N"                  "1 -term"
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


-- ----------------------  dependent-rank

--  dependent-rank /x /y -rank /r

consDepRank :: (Ord c, C.CDec c) => C.RopCons c
consDepRank use =
    do x <- Op.getTerm use "-x"
       y <- Op.getTerm use "-y"
       r <- Op.getTerm use "-rank"
       Right $ relmapDepRank use (x,y,r)

relmapDepRank :: (Ord c, C.CDec c) => C.RopUse c -> B.TermName3 -> C.Relmap c
relmapDepRank use = C.relmapFlow use . relkitDepRank

relkitDepRank :: (Ord c, C.CDec c) => B.TermName3 -> C.RelkitFlow c
relkitDepRank _  Nothing = Right C.relkitNothing
relkitDepRank (x,y,r) (Just he1) = Right kit2 where
    he2         =  B.headFrom [x,r]
    kit2        =  C.relkitJust he2 $ C.RelkitFull False f2
    xyPick      =  Op.picker he1 [x,y]
    f2 bo1      =  map put $ rank $ map get bo1
    get cs      =  let [cx,cy] = xyPick cs in (cx,cy)
    put (cx,i)  =  [cx, C.pDecFromInt i]
    rank        =  Op.depRankList . Op.depRankUpdateAll . Op.depRankFromPairs


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
     Msg.dumpTrees trees


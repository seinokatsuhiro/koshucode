{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Gadget
  ( ropsGadget,
  
    -- * contents
    consContents, relmapContents,
  
    -- * dependent-rank
    consDepRank, relmapDepRank,
  
    -- * visit-distance
    consVisitDistance, relmapVisitDistance,
  
    -- * size
    consSize, relmapSize, relkitSize,
    -- $size
  
    -- * dump-tree
    consDumpTree,
  ) where

import qualified Data.Map.Strict            as Map
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
    --        CONSTRUCTOR       USAGE                      ATTRIBUTE
    [ Op.def  consContents      "contents /N"              "V -term"
    , Op.def  consDumpTree      "dump-tree X"              "V -tree"
    , Op.def  consDepRank       "dependent-rank /P /P -rank /N"     "2 -x -y | -rank"
    , Op.def  consVisitDistance "visit-distance R -step /P ... -to /N -distance /N"  "1 -relmap/ | -step -to -distance"
    , Op.def  consSize          "size /N"                  "1 -term"
    ]


-- ----------------------  contents

consContents :: (Ord c) => C.RopCons c
consContents med =
    do n <- Op.getTerm med "-term"
       Right $ relmapContents med n

relmapContents :: (Ord c) => C.Intmed c -> B.TermName -> C.Relmap c
relmapContents med = C.relmapFlow med . relkitContents

relkitContents :: (Ord c) => B.TermName -> C.RelkitFlow c
relkitContents n _ = Right $ C.relkitJust he2 $ C.RelkitFull False kitf where
    he2  = B.headFrom [n]
    kitf = map B.li1 . B.unique . concat


-- ----------------------  dependent-rank

--  dependent-rank /x /y -rank /r

consDepRank :: (Ord c, C.CDec c) => C.RopCons c
consDepRank med =
    do x <- Op.getTerm med "-x"
       y <- Op.getTerm med "-y"
       r <- Op.getTerm med "-rank"
       Right $ relmapDepRank med (x,y,r)

relmapDepRank :: (Ord c, C.CDec c) => C.Intmed c -> B.TermName3 -> C.Relmap c
relmapDepRank med = C.relmapFlow med . relkitDepRank

relkitDepRank :: (Ord c, C.CDec c) => B.TermName3 -> C.RelkitFlow c
relkitDepRank _  Nothing = Right C.relkitNothing
relkitDepRank (x,y,r) (Just he1) = Right kit2 where
    he2         = B.headFrom [x,r]
    kit2        = C.relkitJust he2 $ C.RelkitFull False f2
    xyPick      = Op.picker he1 [x,y]
    f2 bo1      = map put $ rank $ map get bo1
    get cs      = let [cx,cy] = xyPick cs in (cx,cy)
    put (cx,i)  = [cx, C.pDecFromInt i]
    rank        = Op.depRankList . Op.depRankUpdateAll . Op.depRankFromPairs


-- ----------------------  visit-distance

--  visit-distance r -step /a /b : /c /d -to /v -distance /n

consVisitDistance :: (Ord c, C.CDec c, C.CRel c) => C.RopCons c
consVisitDistance med =
    do rmap  <- Op.getRelmap med "-relmap"
       to    <- Op.getTerm   med "-to"
       dist  <- Op.getTerm   med "-distance"
       steps <- Op.getTermsColon med "-step"
       case steps of
         [step1, step2] -> Right $ relmapVisitDistance med (step1, step2, to, dist) rmap
         _              -> Msg.adlib "Require two sets of terms"

relmapVisitDistance :: (Ord c, C.CDec c, C.CRel c) => C.Intmed c -> ([B.TermName], [B.TermName], B.TermName, B.TermName) -> B.Map (C.Relmap c)
relmapVisitDistance med = C.relmapBinary med . relkitVisitDistance

relkitVisitDistance :: (Ord c, C.CDec c, C.CRel c) => ([B.TermName], [B.TermName], B.TermName, B.TermName) -> C.RelkitBinary c
relkitVisitDistance (step1, step2, to, dist) (C.Relkit _ (Just he2) kitb2) (Just he1)
    | unkStart /= []     = Msg.unkTerm unkStart he1
    | unkFrom  /= []     = Msg.unkTerm unkFrom  he2
    | unkTo    /= []     = Msg.unkTerm unkTo    he2
    | dup      /= []     = Msg.dupAttr dup
    | newDist  == []     = Msg.adlib "Require new term"
    | lenFrom  /= lenTo  = Msg.adlib "Require same number of terms"
    | otherwise          = Right kit3
    where
      lrStart   = step1 `B.headLROrd` B.headNames he1
      lrDist    = [to]  `B.headLROrd` B.headNames he1
      lrFrom    = step1 `B.headLROrd` B.headNames he2
      lrTo      = step2 `B.headLROrd` B.headNames he2

      unkStart  = B.headLSideNames lrStart
      unkFrom   = B.headLSideNames lrFrom
      unkTo     = B.headLSideNames lrTo
      newDist   = B.headLSideNames lrDist

      lenFrom   = length step1
      lenTo     = length step2
      dup       = B.duplicates $ dist : step1
      he3       = B.headConsNest to heTo he1
      heTo      = B.headFrom $ dist : step1
      kit3      = C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]

      kitf3 bmaps bo1 =
          do let [bmap2] = bmaps
             bo2 <- bmap2 bo1
             let vstep = visitStep lrFrom lrTo bo2
             Right $ add vstep `map` bo1

      add vstep cs1 = rel : cs1 where
          start   = B.headRShare lrStart cs1
          vdist   = visitDistanceFrom vstep start
          body    = tuple `map` Map.assocs vdist
          rel     = C.pRel $ B.Rel heTo body
          tuple (cs, n) = C.pDecFromInt n : cs

relkitVisitDistance _ _ _ = Right C.relkitNothing

visitStep :: (Ord c) => B.HeadLR c -> B.HeadLR c -> [[c]] -> VisitStep [c]
visitStep from to = foldr add Map.empty where
    add cs = insertPush (B.headRShare from cs) (B.headRShare to cs)

-- | Mapping node to next nodes.
type VisitStep a = Map.Map a [a]

-- | Mapping node to its distance.
type VisitDist a = Map.Map a Int

visitDistanceFrom :: (Ord a) => VisitStep a -> a -> VisitDist a
visitDistanceFrom step a = visitDistance step $ Map.fromList [(a, 0)]

visitDistance :: (Ord a) => VisitStep a -> VisitDist a -> VisitDist a
visitDistance step = visit 1 where
    sameSize x y = Map.size x == Map.size y
    visit n dist = case visitDistanceStep step dist n of
                     dist' | sameSize dist dist' -> dist'
                           | otherwise           -> visit (n + 1) dist'

visitDistanceStep :: (Ord a) => VisitStep a -> VisitDist a -> Int -> VisitDist a
visitDistanceStep step dist n = foldr insert dist toList where
    insert a  = insertFirst a n
    toList    = next $ Map.keys dist
    next      = concat . B.mapMaybe (`Map.lookup` step)

insertFirst :: (Ord k) => k -> a -> Map.Map k a -> Map.Map k a
insertFirst = Map.insertWith first where
    first _ old = old

insertPush :: (Ord k) => k -> a -> B.Map (Map.Map k [a])
insertPush k a = Map.insertWith push k [a] where
    push _ as = a : as


-- ----------------------  size

-- $size
--
--  Count number of tuples in the output of relmap @a@.
--  
--    > a | size /c
--

consSize :: (C.CDec c) => C.RopCons c
consSize med =
  do n <- Op.getTerm med "-term"
     Right $ relmapSize med n

relmapSize :: (C.CDec c) => C.Intmed c -> B.TermName -> C.Relmap c
relmapSize med n = C.relmapFlow med $ relkitSize n

relkitSize :: (C.CDec c) => B.TermName -> C.RelkitFlow c
relkitSize n _ = Right kit2 where
    he2       = B.headFrom [n]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = [[ C.pDecFromInt $ length bo1 ]]


-- ----------------------  dump-tree

consDumpTree :: (C.CDec c) => C.RopCons c
consDumpTree med =
  do trees <- Op.getTrees med "-tree"
     Msg.dumpTrees trees


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Vanilla.Flow
( -- * enclose
  consEnclose, relmapEnclose, relkitEnclose,
  -- * member
  -- $member
  consMember, relmapMember, relkitMember,
  -- * range
  consRange, relmapRange,
  -- * RDF
  consRdf,
  -- * size
  consSize, relmapSize, relkitSize,
) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Op.Builtin   as Op
import qualified Koshucode.Baala.Op.Minimal   as Op
import qualified Koshucode.Baala.Op.Message   as Message



-- ----------------------  enclose

consEnclose :: (C.CRel c) => C.RopCons c
consEnclose use =
  do n <- Op.getTerm use "-term"
     Right $ relmapEnclose use n

relmapEnclose :: (C.CRel c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapEnclose use = C.relmapFlow use . relkitEnclose

-- | Enclose the current relation in a term.
relkitEnclose :: (C.CRel c) => B.TermName -> C.RelkitCalc c
relkitEnclose _ Nothing = Right C.relkitNothing
relkitEnclose n (Just he1) = Right kit2 where
    he2       = B.Relhead [B.Relnest n $ B.headTerms he1]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = [[ C.pRel $ B.Rel he1 bo1 ]]



-- ----------------------  member

-- $member
--
--  Membership.
--  Relmap @member@ @\/x@ @\/xs@ means
--  @\/x@ is a member of @\/xs@.
--
--  - Input relation has both @\/x@ and @\/xs@,
--    check content of @\/x@ is acturally the member of @\/xs@.
--
--  - Input relation has @\/xs@ and not @\/x@,
--    add term @\/x@ as member of @\/xs@.
--

consMember :: (Ord c, C.CSet c, C.CList c) => C.RopCons c
consMember use =
  do x    <- Op.getTerm use "-1"
     xs   <- Op.getTerm use "-2"
     Right $ relmapMember use (x, xs)

relmapMember :: (Ord c, C.CSet c, C.CList c)
  => C.RopUse c -> B.TermName2 -> C.Relmap c
relmapMember use = C.relmapFlow use . relkitMember

relkitMember :: (Ord c, C.CSet c, C.CList c)
  => B.TermName2 -> C.RelkitCalc c
relkitMember _ Nothing = Right C.relkitNothing
relkitMember (x, xs) he1'@(Just he1) = kit2 where
    kit2 | xi >= 0 && xsi >= 0 = relkitMemberCheck  xi xsi he1'
         | xi <  0 && xsi >= 0 = relkitMemberExpand x  xsi he1'
         | otherwise           = Message.unkTerm [x, xs] he1
    [xi, xsi] = [x, xs] `B.snipFull` B.headNames he1

relkitMemberCheck :: (Eq c, C.CSet c, C.CList c)
  => Int -> Int -> C.RelkitCalc c
relkitMemberCheck xi xsi he1' = Right kit2 where
    kit2 = C.relkit he1' $ C.RelkitPred kitf2
    kitf2 cs = let [xc, xsc] = [xi, xsi] `B.snipFrom` cs
               in xc `C.isMember` xsc

relkitMemberExpand :: (Ord c, C.CSet c, C.CList c)
  => B.TermName -> Int -> C.RelkitCalc c
relkitMemberExpand _ _ Nothing = Right C.relkitNothing
relkitMemberExpand x xsi (Just he1) = Right kit2 where
    he2      = B.headCons x he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToMany False kitf2
    kitf2 cs = let [xsc] = [xsi] `B.snipFrom` cs
               in case xsc of
                    _ | C.isSet  xsc -> map (: cs) $ C.gSet xsc
                    _ | C.isList xsc -> map (: cs) $ B.unique $ C.gList xsc
                    _                -> [xsc : cs]



-- ----------------------  range

consRange :: (C.CDec c) => C.RopCons c
consRange use =
  do term <- Op.getTerm use "-term"
     low  <- Op.getInt  use "-from"
     high <- Op.getInt  use "-to"
     Right $ relmapRange use (term, low, high)

relmapRange :: (C.CDec c) => C.RopUse c -> (B.TermName, Int, Int) -> C.Relmap c
relmapRange use = C.relmapFlow use . relkitRange

relkitRange :: (C.CDec c) => (B.TermName, Int, Int) -> C.RelkitCalc c
relkitRange _ Nothing = Right C.relkitNothing
relkitRange (n, low, high) (Just he1) = Right kit2 where
    he2      = B.headCons n he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToMany False kitf2
    kitf2 cs = map (: cs) decs
    decs     = map C.pDecFromInt [low .. high]



-- ----------------------  RDF

consRdf :: C.RopCons c
consRdf use =
    do sign  <- Op.getWord  use "-pattern"
       [s,o] <- Op.getTerms use "-term"
       Right $ C.relmapSource use sign ["/s", "/o"] `B.mappend`
               Op.relmapRename use [(s,"/s"), (o,"/o")]



-- ----------------------  size

consSize :: (C.CDec c) => C.RopCons c
consSize use =
  do n <- Op.getTerm use "-term"
     Right $ relmapSize use n

relmapSize :: (C.CDec c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapSize use n = C.relmapFlow use $ relkitSize n

-- | Cardinality
relkitSize :: (C.CDec c) => B.TermName -> C.RelkitCalc c
relkitSize n _ = Right kit2 where
    he2       = B.headFrom [n]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = [[ C.pDecFromInt $ length bo1 ]]


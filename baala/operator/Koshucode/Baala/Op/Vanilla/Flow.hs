{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Vanilla.Flow
( 
  -- * const
  consConst, relmapConst, relkitConst,
  -- $const

  -- * member
  consMember, relmapMember, relkitMember,
  -- $member

  -- * range
  consRange, relmapRange,
  -- $range

  -- * RDF
  consRdf,
  -- * size
  consSize, relmapSize, relkitSize,
  -- $size

  -- * assn
  consAssn, relmapAssn, relkitAssn,
  -- * unassn
  consUnassn, relmapUnassn, relkitUnassn,
) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Op.Builtin   as Op
import qualified Koshucode.Baala.Op.Minimal   as Op
import qualified Koshucode.Baala.Op.Message   as Message



-- ----------------------  const

-- $const
--
--  Same as relmap @dee@
--  
--    > const {| | |}
--
--  Same as relmap @dum@
--  
--    > const {| |}

consConst :: (C.CContent c) => C.RopCons c
consConst use =
    do tree <- Op.getTree use "-lit"
       lit  <- C.litContent tree
       case C.isRel lit of
         True  -> Right $ relmapConst use $ C.gRel lit
         False -> Message.reqRel

relmapConst :: C.RopUse c -> B.Rel c -> C.Relmap c
relmapConst use = C.relmapFlow use . relkitConst

relkitConst :: B.Rel c -> C.RelkitFlow c
relkitConst _ Nothing = Right C.relkitNothing
relkitConst (B.Rel he bo) _ = Right kit2 where
    kit2 = C.relkitJust he $ C.RelkitConst bo


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

consMember :: (Ord c, C.CSet c, C.CList c, C.CText c) => C.RopCons c
consMember use =
  do x    <- Op.getTerm use "-1"
     xs   <- Op.getTerm use "-2"
     Right $ relmapMember use (x, xs)

relmapMember :: (Ord c, C.CSet c, C.CList c, C.CText c)
  => C.RopUse c -> B.TermName2 -> C.Relmap c
relmapMember use = C.relmapFlow use . relkitMember

relkitMember :: (Ord c, C.CSet c, C.CList c, C.CText c)
  => B.TermName2 -> C.RelkitFlow c
relkitMember _ Nothing = Right C.relkitNothing
relkitMember (x, xs) he1'@(Just he1) = kit2 where
    kit2 | B.operand [xi, xsi] [] = relkitMemberCheck  xi xsi he1'
         | B.operand [xsi] [xi]   = relkitMemberExpand x  xsi he1'
         | otherwise              = Message.unkTerm [x, xs] he1
    [xi, xsi] = [x, xs] `B.snipFull` B.headNames he1

relkitMemberCheck :: (Eq c, C.CSet c, C.CList c)
  => Int -> Int -> C.RelkitFlow c
relkitMemberCheck xi xsi he1' = Right kit2 where
    kit2 = C.relkit he1' $ C.RelkitPred kitf2
    kitf2 cs = let [xc, xsc] = [xi, xsi] `B.snipFrom` cs
               in xc `C.isMember` xsc

relkitMemberExpand :: (Ord c, C.CSet c, C.CList c, C.CText c)
  => B.TermName -> Int -> C.RelkitFlow c
relkitMemberExpand _ _ Nothing = Right C.relkitNothing
relkitMemberExpand x xsi (Just he1) = Right kit2 where
    he2      = B.headCons x he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToMany False kitf2
    kitf2 cs = let [xsc] = [xsi] `B.snipFrom` cs
               in case xsc of
                    _ | C.isSet  xsc -> map (: cs) $ C.gSet xsc
                    _ | C.isList xsc -> map (: cs) $ B.unique $ C.gList xsc
                    _ | C.isText xsc -> map (: cs) $ map (C.pText . B.singleton)
                                                   $ B.unique $ C.gText xsc
                    _                -> [xsc : cs]



-- ----------------------  range

-- $range
--
--  Add term @\/n@ @0@, @\/n@ @1@, ..., and @\/n@ @9@.
--  
--    > range /n -from 0 -to 9

consRange :: (C.CDec c) => C.RopCons c
consRange use =
  do term <- Op.getTerm use "-term"
     low  <- Op.getInt  use "-from"
     high <- Op.getInt  use "-to"
     Right $ relmapRange use (term, low, high)

relmapRange :: (C.CDec c) => C.RopUse c -> (B.TermName, Int, Int) -> C.Relmap c
relmapRange use = C.relmapFlow use . relkitRange

relkitRange :: (C.CDec c) => (B.TermName, Int, Int) -> C.RelkitFlow c
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

-- $size
--
--  Count number of tuples in the output of relmap @a@.
--  
--    > a | size /c

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


-- ----------------------  assn

--    > assn /x /y /z -to /a

consAssn :: (C.CTermset c) => C.RopCons c
consAssn use =
  do ns <- Op.getTerms use "-term"
     to <- Op.getTerm  use "-to"
     Right $ relmapAssn use (ns, to)

relmapAssn :: (C.CTermset c) => C.RopUse c -> ([B.TermName], B.TermName) -> C.Relmap c
relmapAssn use = C.relmapFlow use . relkitAssn

relkitAssn :: (C.CTermset c) => ([B.TermName], B.TermName) -> C.RelkitFlow c
relkitAssn _ Nothing = Right C.relkitNothing
relkitAssn (ns, to) (Just he1) = Right kit2 where
    ns1       =  B.headNames he1
    ind       =  B.snipIndex ns ns1
    he2       =  B.headCons to he1
    kit2      =  C.relkitJust he2 $ C.RelkitOneToOne False f2
    f2 cs1    =  let cs   = B.snipFrom ind cs1
                     assn = C.pTermset $ zip ns cs
                 in assn : cs1


-- ----------------------  unassn

--    > unassn /a

consUnassn :: (C.CTermset c) => C.RopCons c
consUnassn use =
  do from <- Op.getTerm  use "-from"
     ns   <- Op.getTerms use "-only"
     Right $ relmapUnassn use (from, ns)

relmapUnassn :: (C.CTermset c) => C.RopUse c -> (B.TermName, [B.TermName]) -> C.Relmap c
relmapUnassn use = C.relmapFlow use . relkitUnassn

relkitUnassn :: (C.CTermset c) => (B.TermName, [B.TermName]) -> C.RelkitFlow c
relkitUnassn _ Nothing = Right C.relkitNothing
relkitUnassn (from, ns) (Just he1) = Right kit2 where
    ns1       =  B.headNames he1
    ind       =  B.snipIndex [from] ns1
    he2       =  B.headAppend ns he1
    kit2      =  C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
    f2 _ cs1  =  do let [assn] = B.snipFrom ind cs1
                    cs <- assnPick ns $ C.gTermset assn
                    Right $ cs ++ cs1

assnPick :: (Eq a) => [a] -> [(a, b)] -> B.Ab [b]
assnPick ns assn = mapM pick ns where
    pick n = case lookup n assn of
               Just c   ->  Right c
               Nothing  ->  Message.adlib "no term"


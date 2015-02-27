{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Peripheral
  ( ropsPeripheral,
  
    -- * member
    consMember, relmapMember, relkitMember,
    -- $member
  
    -- * RDF
    consRdf,
  
    -- * assn
    consAssn, relmapAssn, relkitAssn,
  
    -- * unassn
    consUnassn, relmapUnassn, relkitUnassn,
  
    -- * term-name
    consTermName, relmapTermName, relkitTermName,
  
    -- * today
    relmapToday, relkitToday,
  ) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Op.Builtin   as Op
import qualified Koshucode.Baala.Op.Term      as Op
import qualified Koshucode.Baala.Op.Message   as Msg

-- | Implementation of relational operators.
--
--   [@member \/N \/N@]
--     Membership of set or list.
-- 
--   [@rdf P \/S \/O@]
--     Retrieve relation from RDF-like judgements.
-- 
ropsPeripheral :: (C.CContent c) => [C.Rop c]
ropsPeripheral = Op.ropList "peripheral"
    --       CONSTRUCTOR   USAGE                     ATTRIBUTE
    [ Op.def consAssn      "assn /P ... -to N"       "V -term | -to"
    , Op.def consMember    "member /N /N"            "E -1 -2"
    , Op.def consRdf       "rdf P /S /O"             "1V -pattern -term"
    , Op.def consTermName  "term-name /N"            "1 -term"
    , Op.def consToday     "today /N"                "1 -term"
    , Op.def consUnassn    "unassn /P -only /P ..."  "1 -from | -only"
    ]

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
consMember med =
  do x    <- Op.getTerm med "-1"
     xs   <- Op.getTerm med "-2"
     Right $ relmapMember med (x, xs)

relmapMember :: (Ord c, C.CSet c, C.CList c, C.CText c)
  => C.Intmed c -> B.TermName2 -> C.Relmap c
relmapMember med = C.relmapFlow med . relkitMember

relkitMember :: (Ord c, C.CSet c, C.CList c, C.CText c)
  => B.TermName2 -> C.RelkitFlow c
relkitMember _ Nothing = Right C.relkitNothing
relkitMember (x, xs) he1'@(Just he1) = kit2 where
    kit2 | B.operand [xi, xsi] [] = relkitMemberCheck  xi xsi he1'
         | B.operand [xsi] [xi]   = relkitMemberExpand x  xsi he1'
         | otherwise              = Msg.unkTerm [x, xs] he1
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
                    _ | C.isText xsc -> map (: cs) $ map (C.pText . B.li1)
                                                   $ B.unique $ C.gText xsc
                    _                -> [xsc : cs]


-- ----------------------  RDF

consRdf :: C.RopCons c
consRdf med =
    do sign  <- Op.getWord  med "-pattern"
       [s,o] <- Op.getTerms med "-term"
       Right $ C.relmapSource med sign ["/s", "/o"] `B.mappend`
               Op.relmapRename med [(s,"/s"), (o,"/o")]



-- ----------------------  assn

--    > assn /x /y /z -to /a

consAssn :: (C.CAssn c) => C.RopCons c
consAssn med =
  do ns <- Op.getTerms med "-term"
     to <- Op.getTerm  med "-to"
     Right $ relmapAssn med (ns, to)

relmapAssn :: (C.CAssn c) => C.Intmed c -> ([B.TermName], B.TermName) -> C.Relmap c
relmapAssn med = C.relmapFlow med . relkitAssn

relkitAssn :: (C.CAssn c) => ([B.TermName], B.TermName) -> C.RelkitFlow c
relkitAssn _ Nothing = Right C.relkitNothing
relkitAssn (ns, to) (Just he1) = Right kit2 where
    pick      =  Op.picker he1 ns
    he2       =  B.headCons to he1
    kit2      =  C.relkitJust he2 $ C.RelkitOneToOne False f2
    f2 cs1    =  let assn = C.pAssn $ zip ns $ pick cs1
                 in assn : cs1


-- ----------------------  unassn

--    > unassn /a -only /x /y

consUnassn :: (C.CAssn c) => C.RopCons c
consUnassn med =
  do from <- Op.getTerm  med "-from"
     ns   <- Op.getTerms med "-only"
     Right $ relmapUnassn med (from, ns)

relmapUnassn :: (C.CAssn c) => C.Intmed c -> (B.TermName, [B.TermName]) -> C.Relmap c
relmapUnassn med = C.relmapFlow med . relkitUnassn

relkitUnassn :: (C.CAssn c) => (B.TermName, [B.TermName]) -> C.RelkitFlow c
relkitUnassn _ Nothing = Right C.relkitNothing
relkitUnassn (from, ns) (Just he1) = Right kit2 where
    pick      =  Op.picker he1 [from]
    he2       =  B.headAppend ns he1
    kit2      =  C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
    f2 _ cs1  =  do let [assn] = pick cs1
                    cs <- assnPick ns $ C.gAssn assn
                    Right $ cs ++ cs1

assnPick :: [B.TermName] -> [B.Named c] -> B.Ab [c]
assnPick ns assn = mapM pick ns where
    pick n = case lookup n assn of
               Just c   ->  Right c
               Nothing  ->  Msg.adlib "no term"


-- ----------------------  term-name

consTermName :: (C.CTerm c) => C.RopCons c
consTermName med =
  do n <- Op.getTerm med "-term"
     Right $ relmapTermName med n

relmapTermName :: (C.CTerm c) => C.Intmed c -> B.TermName -> C.Relmap c
relmapTermName med n = C.relmapFlow med $ relkitTermName n

relkitTermName :: (C.CTerm c) => B.TermName -> C.RelkitFlow c
relkitTermName n Nothing    = Msg.noAttr n
relkitTermName n (Just he1) = Right kit2 where
    he2       = B.headFrom [n]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 _   = map term $ B.headNames he1
    term t    = [C.pTerm t]


-- ----------------------  today

--  today /day

consToday :: (C.CTime c) => C.RopCons c
consToday med =
  do n <- Op.getTerm med "-term"
     let t = C.globalTime $ C.ropGlobal med
     Right $ relmapToday med (n, t)

relmapToday :: (C.CTime c) => C.Intmed c -> (B.TermName, B.Time) -> C.Relmap c
relmapToday med = C.relmapFlow med . relkitToday

relkitToday :: (C.CTime c) => (B.TermName, B.Time) -> Maybe B.Head -> B.Ab (C.Relkit c)
relkitToday _ Nothing = Right C.relkitNothing
relkitToday (n, t) (Just he1) = Right kit2 where
    he2   = B.headCons n he1
    kit2  = C.relkitJust he2 $ C.RelkitOneToOne False f2
    f2 cs = C.pTime t : cs


{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Peripheral
  ( ropsPeripheral,
  
    -- * member
    consMember, relmapMember, relkitMember,
    -- $member
  
    -- * index-elem
    consIndexElem, relmapIndexElem, relkitIndexElem,

    -- * uncollect
    consUncollect, relmapUncollect, relkitUncollect,

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
    --       CONSTRUCTOR   USAGE                      ATTRIBUTE
    [ Op.def consAssn      "assn /P ... -to N"        "V -term | -to"
    , Op.def consIndexElem "index-elem /N /N /P"      "3 -index -elem -list"
    , Op.def consMember    "member /N /N"             "E -1 -2"
    , Op.def consRdf       "rdf P /S /O"              "1V -pattern -term"
    , Op.def consTermName  "term-name /N"             "1 -term"
    , Op.def consToday     "today /N"                 "1 -term"
    , Op.def consUnassn    "unassn /P -only /P ..."   "1 -from | -only"
    , Op.def consUncollect "uncollect /P -to /N ..."  "1 -coll | -to"
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
    kit2 | [xi, xsi] B.+- []   = relkitMemberCheck  xi xsi he1'
         | [xsi]     B.+- [xi] = relkitMemberExpand x  xsi he1'
         | otherwise            = Msg.unkTerm [x, xs] he1
    [xi, xsi] = headIndex he1 [x, xs]

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


-- ----------------------  index-elem

consIndexElem :: (Ord c, C.CSet c, C.CList c, C.CText c, C.CDec c) => C.RopCons c
consIndexElem med =
  do i   <- Op.getTerm med "-index"
     x   <- Op.getTerm med "-elem"
     xs  <- Op.getTerm med "-list"
     Right $ relmapIndexElem med (i, x, xs)

relmapIndexElem :: (Ord c, C.CSet c, C.CList c, C.CText c, C.CDec c)
  => C.Intmed c -> B.TermName3 -> C.Relmap c
relmapIndexElem med = C.relmapFlow med . relkitIndexElem

relkitIndexElem :: (Ord c, C.CSet c, C.CList c, C.CText c, C.CDec c)
  => B.TermName3 -> C.RelkitFlow c
relkitIndexElem _ Nothing = Right C.relkitNothing
relkitIndexElem (i, x, xs) he1'@(Just he1) = kit2 where
    kit2 | B.termsPN [xsi] [xi, ii]  = relkitIndexElemExpand i x xsi he1'
         | otherwise                 = Msg.unkTerm [i, x, xs] he1
    [ii, xi, xsi] = headIndex he1 [i, x, xs]

headIndex :: B.Head -> [B.TermName] -> [Int]
headIndex he ns = ns `B.snipFull` B.headNames he

relkitIndexElemExpand :: forall c. (Ord c, C.CSet c, C.CList c, C.CText c, C.CDec c)
  => B.TermName -> B.TermName -> Int -> C.RelkitFlow c
relkitIndexElemExpand _ _ _ Nothing = Right C.relkitNothing
relkitIndexElemExpand i x xsi (Just he1) = Right kit2 where
    he2      = B.headAppend [i, x] he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToMany False kitf2
    kitf2 cs = let [xsc] = [xsi] `B.snipFrom` cs
               in case xsc of
                    _ | C.isSet  xsc -> indexElem cs $ B.sort $ C.gSet xsc
                    _ | C.isList xsc -> indexElem cs $ C.gList xsc
                    _ | C.isText xsc -> indexElem cs $ map (C.pText . B.li1)
                                                     $ C.gText xsc
                    _                -> [xsc : cs]

    indexElem :: [c] -> [c] -> [[c]]
    indexElem cs = map (cons cs) . index

    index :: [c] -> [(c, c)]
    index = zip $ map C.pDecFromInt [1 ..]

    cons :: [c] -> (c, c) -> [c]
    cons cs (ic, xc) = ic : xc : cs


-- ----------------------  uncollect

consUncollect :: (Ord c, C.CSet c, C.CList c, C.CText c, C.CDec c, C.CEmpty c) => C.RopCons c
consUncollect med =
  do coll  <- Op.getTerm  med "-coll"
     to    <- Op.getTerms med "-to"
     Right $ relmapUncollect med (coll, to)

relmapUncollect :: (Ord c, C.CSet c, C.CList c, C.CText c, C.CDec c, C.CEmpty c)
  => C.Intmed c -> (B.TermName, [B.TermName]) -> C.Relmap c
relmapUncollect med = C.relmapFlow med . relkitUncollect

relkitUncollect :: (Ord c, C.CSet c, C.CList c, C.CText c, C.CDec c, C.CEmpty c)
  => (B.TermName, [B.TermName]) -> C.RelkitFlow c
relkitUncollect _ Nothing = Right C.relkitNothing
relkitUncollect (coll, to) (Just he1) = kit2 where
    kit2 | B.termsPN icoll ito  = Right $ C.relkitJust he2 $ C.RelkitOneToOne False kitf2
         | otherwise            = Msg.unkTerm (coll : to) he1

    icoll    = headIndex he1 [coll]
    ito      = headIndex he1 to
    he2      = B.headAppend to he1
    kitf2 cs = let [xsc]    = icoll `B.snipFrom` cs
                   char     = C.pText . B.li1
                   ys << xs = appendCount C.empty (length to) xs ys
               in case () of
                    _ | C.isSet  xsc  -> cs << (B.sort $ C.gSet xsc)
                      | C.isList xsc  -> cs << (C.gList xsc)
                      | C.isText xsc  -> cs << (map char $ C.gText xsc)
                      | otherwise     -> cs << []

appendCount :: a -> Int -> [a] -> [a] -> [a]
appendCount fill num xs ys = loop num xs where
    loop 0 _        = ys
    loop n (x:xs2)  = x    : loop (n - 1) xs2
    loop n []       = fill : loop (n - 1) []


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


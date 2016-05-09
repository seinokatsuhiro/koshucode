{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Flat.Peripheral
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
  
    -- * tie
    consTie, relmapTie, relkitTie,
  
    -- * untie
    consUntie, relmapUntie, relkitUntie,
  
    -- * term-name
    consTermName, relmapTermName, relkitTermName,
  
    -- * today
    relmapToday, relkitToday,
  ) where

import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Syntax             as S
import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Op
import qualified Koshucode.Baala.Rop.Flat.Term      as Op
import qualified Koshucode.Baala.Rop.Flat.Message   as Msg

-- | Implementation of relational operators.
--
--   [@member \/N \/N@]
--     Membership of set or list.
-- 
--   [@rdf P \/S \/O@]
--     Retrieve relation from RDF-like judgements.
-- 
ropsPeripheral :: (D.CContent c) => [C.Rop c]
ropsPeripheral = Op.ropList "peripheral"
    --       CONSTRUCTOR   USAGE                      ATTRIBUTE
    [ Op.def consTie       "tie /P ... -to N"         "* -term . -to"
    , Op.def consIndexElem "index-elem /N /N /P"      "3 -index -elem -list"
    , Op.def consMember    "member /N /N"             "2 -elem -set"
    , Op.def consRdf       "rdf P /S /O"              "1* -pattern -term"
    , Op.def consTermName  "term-name /N"             "1 -term"
    , Op.def consToday     "today /N"                 "1 -term"
    , Op.def consUntie     "untie /P -only /P ..."    "1 -from . -only"
    , Op.def consUncollect "uncollect /P -to /N ..."  "1 -coll . -to"
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

consMember :: (Ord c, D.CSet c, D.CList c, D.CText c) => C.RopCons c
consMember med =
  do x    <- Op.getTerm med "-elem"
     xs   <- Op.getTerm med "-set"
     Right $ relmapMember med (x, xs)

relmapMember :: (Ord c, D.CSet c, D.CList c, D.CText c)
  => C.Intmed c -> S.TermName2 -> C.Relmap c
relmapMember med = C.relmapFlow med . relkitMember

relkitMember :: (Ord c, D.CSet c, D.CList c, D.CText c)
  => S.TermName2 -> C.RelkitFlow c
relkitMember _ Nothing = Right C.relkitNothing
relkitMember (x, xs) he1'@(Just he1) = kit2 where
    kit2 | [xi, xsi] B.+- []   = relkitMemberCheck  xi xsi he1'
         | [xsi]     B.+- [xi] = relkitMemberExpand x  xsi he1'
         | otherwise            = Msg.unkTerm [x, xs] he1
    [xi, xsi] = headIndex he1 [x, xs]

relkitMemberCheck :: (Eq c, D.CSet c, D.CList c)
  => Int -> Int -> C.RelkitFlow c
relkitMemberCheck xi xsi he1' = Right kit2 where
    kit2 = C.relkit he1' $ C.RelkitPred kitf2
    kitf2 cs = let [xc, xsc] = [xi, xsi] `B.snipFrom` cs
               in xc `D.isMember` xsc

relkitMemberExpand :: (Ord c, D.CSet c, D.CList c, D.CText c)
  => S.TermName -> Int -> C.RelkitFlow c
relkitMemberExpand _ _ Nothing = Right C.relkitNothing
relkitMemberExpand x xsi (Just he1) = Right kit2 where
    he2      = D.headCons x he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToMany False kitf2
    kitf2 cs = let [xsc] = [xsi] `B.snipFrom` cs
               in case xsc of
                    _ | D.isSet  xsc -> map (: cs) $ D.gSet xsc
                    _ | D.isList xsc -> map (: cs) $ B.unique $ D.gList xsc
                    _ | D.isText xsc -> map (: cs) $ map (D.pText . B.li1)
                                                   $ B.unique $ D.gText xsc
                    _                -> [xsc : cs]


-- ----------------------  index-elem

consIndexElem :: (Ord c, D.CSet c, D.CList c, D.CText c, D.CDec c) => C.RopCons c
consIndexElem med =
  do i   <- Op.getTerm med "-index"
     x   <- Op.getTerm med "-elem"
     xs  <- Op.getTerm med "-list"
     Right $ relmapIndexElem med (i, x, xs)

relmapIndexElem :: (Ord c, D.CSet c, D.CList c, D.CText c, D.CDec c)
  => C.Intmed c -> S.TermName3 -> C.Relmap c
relmapIndexElem med = C.relmapFlow med . relkitIndexElem

relkitIndexElem :: (Ord c, D.CSet c, D.CList c, D.CText c, D.CDec c)
  => S.TermName3 -> C.RelkitFlow c
relkitIndexElem _ Nothing = Right C.relkitNothing
relkitIndexElem (i, x, xs) he1'@(Just he1) = kit2 where
    kit2 | S.termsPN [xsi] [xi, ii]  = relkitIndexElemExpand i x xsi he1'
         | otherwise                 = Msg.unkTerm [i, x, xs] he1
    [ii, xi, xsi] = headIndex he1 [i, x, xs]

headIndex :: D.Head -> [S.TermName] -> [Int]
headIndex he ns = ns `B.snipFull` D.headNames he

relkitIndexElemExpand :: forall c. (Ord c, D.CSet c, D.CList c, D.CText c, D.CDec c)
  => S.TermName -> S.TermName -> Int -> C.RelkitFlow c
relkitIndexElemExpand _ _ _ Nothing = Right C.relkitNothing
relkitIndexElemExpand i x xsi (Just he1) = Right kit2 where
    he2      = D.headAppend [i, x] he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToMany False kitf2
    kitf2 cs = let [xsc] = [xsi] `B.snipFrom` cs
               in case xsc of
                    _ | D.isSet  xsc -> indexElem cs $ B.sort $ D.gSet xsc
                    _ | D.isList xsc -> indexElem cs $ D.gList xsc
                    _ | D.isText xsc -> indexElem cs $ map (D.pText . B.li1)
                                                     $ D.gText xsc
                    _                -> [xsc : cs]

    indexElem :: [c] -> [c] -> [[c]]
    indexElem cs = map (cons cs) . index

    index :: [c] -> [(c, c)]
    index = zip $ map D.pInt [1 ..]

    cons :: [c] -> (c, c) -> [c]
    cons cs (ic, xc) = ic : xc : cs


-- ----------------------  uncollect

consUncollect :: (Ord c, D.CSet c, D.CList c, D.CText c, D.CDec c, D.CEmpty c) => C.RopCons c
consUncollect med =
  do coll  <- Op.getTerm  med "-coll"
     to    <- Op.getTerms med "-to"
     Right $ relmapUncollect med (coll, to)

relmapUncollect :: (Ord c, D.CSet c, D.CList c, D.CText c, D.CDec c, D.CEmpty c)
  => C.Intmed c -> (S.TermName, [S.TermName]) -> C.Relmap c
relmapUncollect med = C.relmapFlow med . relkitUncollect

relkitUncollect :: (Ord c, D.CSet c, D.CList c, D.CText c, D.CDec c, D.CEmpty c)
  => (S.TermName, [S.TermName]) -> C.RelkitFlow c
relkitUncollect _ Nothing = Right C.relkitNothing
relkitUncollect (coll, to) (Just he1) = kit2 where
    kit2 | S.termsPN icoll ito  = Right $ C.relkitJust he2 $ C.RelkitOneToOne False kitf2
         | otherwise            = Msg.unkTerm (coll : to) he1

    icoll    = headIndex he1 [coll]
    ito      = headIndex he1 to
    he2      = D.headAppend to he1
    kitf2 cs = let [xsc]    = icoll `B.snipFrom` cs
                   char     = D.pText . B.li1
                   ys << xs = appendCount D.empty (length to) xs ys
               in case () of
                    _ | D.isSet  xsc  -> cs << (B.sort $ D.gSet xsc)
                      | D.isList xsc  -> cs << (D.gList xsc)
                      | D.isText xsc  -> cs << (map char $ D.gText xsc)
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



-- ----------------------  tie

--    > tie /x /y /z -to /a

consTie :: (D.CTie c) => C.RopCons c
consTie med =
  do ns <- Op.getTerms med "-term"
     to <- Op.getTerm  med "-to"
     Right $ relmapTie med (ns, to)

relmapTie :: (D.CTie c) => C.Intmed c -> ([S.TermName], S.TermName) -> C.Relmap c
relmapTie med = C.relmapFlow med . relkitTie

relkitTie :: (D.CTie c) => ([S.TermName], S.TermName) -> C.RelkitFlow c
relkitTie _ Nothing = Right C.relkitNothing
relkitTie (ns, to) (Just he1) = Right kit2 where
    pick      =  Op.picker he1 ns
    he2       =  D.headCons to he1
    kit2      =  C.relkitJust he2 $ C.RelkitOneToOne False f2
    f2 cs1    =  let tie = D.pTie $ zip ns $ pick cs1
                 in tie : cs1


-- ----------------------  untie

--    > untie /a -only /x /y

consUntie :: (D.CTie c) => C.RopCons c
consUntie med =
  do from <- Op.getTerm  med "-from"
     ns   <- Op.getTerms med "-only"
     Right $ relmapUntie med (from, ns)

relmapUntie :: (D.CTie c) => C.Intmed c -> (S.TermName, [S.TermName]) -> C.Relmap c
relmapUntie med = C.relmapFlow med . relkitUntie

relkitUntie :: (D.CTie c) => (S.TermName, [S.TermName]) -> C.RelkitFlow c
relkitUntie _ Nothing = Right C.relkitNothing
relkitUntie (from, ns) (Just he1) = Right kit2 where
    pick      =  Op.picker he1 [from]
    he2       =  D.headAppend ns he1
    kit2      =  C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
    f2 _ cs1  =  do let [tie] = pick cs1
                    cs <- tiePick ns $ D.gTie tie
                    Right $ cs ++ cs1

tiePick :: [S.TermName] -> [S.Term c] -> B.Ab [c]
tiePick ns tie = mapM pick ns where
    pick n = case lookup n tie of
               Just c   ->  Right c
               Nothing  ->  Msg.adlib "no term"


-- ----------------------  term-name

consTermName :: (D.CTerm c) => C.RopCons c
consTermName med =
  do n <- Op.getTerm med "-term"
     Right $ relmapTermName med n

relmapTermName :: (D.CTerm c) => C.Intmed c -> S.TermName -> C.Relmap c
relmapTermName med n = C.relmapFlow med $ relkitTermName n

relkitTermName :: (D.CTerm c) => S.TermName -> C.RelkitFlow c
relkitTermName n Nothing    = Msg.noAttr n
relkitTermName n (Just he1) = Right kit2 where
    he2       = D.headFrom [n]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 _   = map term $ D.headNames he1
    term t    = [D.pTerm t]


-- ----------------------  today

--  today /day

consToday :: (D.CTime c) => C.RopCons c
consToday med =
  do n <- Op.getTerm med "-term"
     let t = C.globalTime $ C.ropGlobal med
     Right $ relmapToday med (n, t)

relmapToday :: (D.CTime c) => C.Intmed c -> (S.TermName, D.Time) -> C.Relmap c
relmapToday med = C.relmapFlow med . relkitToday

relkitToday :: (D.CTime c) => (S.TermName, D.Time) -> Maybe D.Head -> B.Ab (C.Relkit c)
relkitToday _ Nothing = Right C.relkitNothing
relkitToday (n, t) (Just he1) = Right kit2 where
    he2   = D.headCons n he1
    kit2  = C.relkitJust he2 $ C.RelkitOneToOne False f2
    f2 cs = D.pTime t : cs


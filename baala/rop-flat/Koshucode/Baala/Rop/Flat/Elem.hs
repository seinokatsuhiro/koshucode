{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Flat.Elem
  ( ropsElem,
  
    -- * member
    consMember, relmapMember, relkitMember,
    -- $member
  
    -- * ix-elem & iz-elem
    consIndexElem, relmapIndexElem, relkitIndexElem,

    -- * unroll
    consUnroll, relmapUnroll,

    -- * elem-begin
    consElemBegin, relmapElemBegin,
    -- * elem-end
    consElemEnd, relmapElemEnd,
    -- * uncollect
    consUncollect, relmapUncollect, relkitUncollect,
  ) where

import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Syntax             as S
import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Op
import qualified Koshucode.Baala.Rop.Flat.Message   as Msg

-- | Implementation of relational operators.
ropsElem :: (D.CContent c) => [C.Rop c]
ropsElem = Op.ropList "elem"
    --       CONSTRUCTOR       USAGE                        ATTRIBUTE
    [ Op.def consElem          "elem /P -to /N"             "-coll . -to"
    , Op.def consElemBegin     "elem-begin /P -to /N ..."   "-coll . -to"
    , Op.def consElemEnd       "elem-end /P -to /N ..."     "-coll . -to"
    , Op.def (consIndexElem 1) "ix-elem /P -to /N /N"       "-coll . -to"
    , Op.def (consIndexElem 0) "iz-elem /P -to /N /N"       "-coll . -to"
    , Op.def consMember        "member /N /N"               "-elem -set"
    , Op.def consUncollect     "uncollect /P -to /N ..."    "-coll . -to"
    , Op.def consUnroll        "unroll /N /N -from /P ..."  "-term -content . -from"
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

-- | Expand elements from collection.
--
--   >>> elem /list -to /x

consElem :: (Ord c, D.CSet c, D.CList c, D.CText c) => C.RopCons c
consElem med =
  do xs   <- Op.getTerm med "-coll"
     x    <- Op.getTerm med "-to"
     Right $ relmapMember med (x, xs)

-- | Expand elements from collection.
consMember :: (Ord c, D.CSet c, D.CList c, D.CText c) => C.RopCons c
consMember med =
  do x    <- Op.getTerm med "-elem"
     xs   <- Op.getTerm med "-set"
     Right $ relmapMember med (x, xs)

-- | Create @member@ relmap.
relmapMember :: (Ord c, D.CSet c, D.CList c, D.CText c)
  => C.Intmed c -> S.TermName2 -> C.Relmap c
relmapMember med = C.relmapFlow med . relkitMember

-- | Create @member@ relkit.
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

-- | Expand index and element from collection.
--
--   >>> ix-elem /list -to /i /elem
--   >>> iz-elem /list -to /i /elem
--
consIndexElem :: (Ord c, D.CContent c) => Int -> C.RopCons c
consIndexElem from med =
  do xs     <- Op.getTerm  med "-coll"
     (i, x) <- Op.getTerm2 med "-to"
     Right $ relmapIndexElem from med (i, x, xs)

-- | Create @ix-elem@ or @iz-elem@ relmap.
relmapIndexElem :: (Ord c, D.CContent c) => Int -> C.Intmed c -> S.TermName3 -> C.Relmap c
relmapIndexElem from med = C.relmapFlow med . relkitIndexElem from

-- | Create @ix-elem@ or @iz-elem@ relkit.
relkitIndexElem :: (Ord c, D.CContent c) => Int -> S.TermName3 -> C.RelkitFlow c
relkitIndexElem _ _ Nothing = Right C.relkitNothing
relkitIndexElem from (i, x, xs) he1'@(Just he1) = kit2 where
    kit2 | S.termsPN [xsi] [xi, ii]  = relkitIndexElemExpand from i x xsi he1'
         | otherwise                 = Msg.unkTerm [i, x, xs] he1
    [ii, xi, xsi] = headIndex he1 [i, x, xs]

headIndex :: D.Head -> [S.TermName] -> [Int]
headIndex he ns = ns `B.snipIndexFull` D.getTermNames he

relkitIndexElemExpand :: forall c. (Ord c, D.CContent c)
  => Int -> S.TermName -> S.TermName -> Int -> C.RelkitFlow c
relkitIndexElemExpand _ _ _ _ Nothing = Right C.relkitNothing
relkitIndexElemExpand from i x xsi (Just he1) = Right kit2 where
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
    index = zip $ map D.pInt [from ..]

    cons :: [c] -> (c, c) -> [c]
    cons cs (ic, xc) = ic : xc : cs


-- ----------------------  unroll

-- | Unroll (expand) terms to tuples.
--
--   >>> unroll /term /content -from /1 /2 /3 /4

consUnroll :: (D.CTerm c) => C.RopCons c
consUnroll med =
  do t   <- Op.getTerm  med "-term"
     c   <- Op.getTerm  med "-content"
     xs  <- Op.getTerms med "-from"
     Right $ relmapUnroll med (t, c, xs)

-- | Create @unroll@ relmap.
relmapUnroll :: (D.CTerm c) => C.Intmed c -> (S.TermName, S.TermName, [S.TermName]) -> C.Relmap c
relmapUnroll med = C.relmapFlow med . relkitUnroll

-- | Create @unroll@ relkit.
relkitUnroll :: (D.CTerm c) =>  (S.TermName, S.TermName, [S.TermName]) -> C.RelkitFlow c
relkitUnroll _ Nothing = Right C.relkitNothing
relkitUnroll (t, c, from) (Just he1) = kit2 where
    kit2 | S.termsPN fromi [ti, ci]  = Right $ C.relkitJust he2 $ C.RelkitOneToMany False kitf2
         | otherwise                 = Msg.unkTerm (t : c : from) he1
    [ti, ci] = headIndex he1 [t, c]
    fromi    = headIndex he1 from
    he2      = D.headAppend [t, c] $ D.headMap (B.snipOff fromi) he1
    kitf2 cs = let (fromc, cs') = fromi `B.snipBoth` cs
                   cons (term, cont) = D.pTerm term : cont : cs'
               in cons <$> zip from fromc


-- ----------------------  elem-begin

-- | Extract elements from collection.
--
--   >>> elem-begin /list -to /a /b /c

consElemBegin :: (Ord c, D.CContent c) => C.RopCons c
consElemBegin med =
  do coll <- Op.getTerm  med "-coll"
     to   <- Op.getTerms med "-to"
     Right $ relmapElemBegin med (coll, to)

-- | Create @elem-begin@ relmap.
relmapElemBegin :: (Ord c, D.CContent c) => C.Intmed c -> (S.TermName, [S.TermName]) -> C.Relmap c
relmapElemBegin med = C.relmapFlow med . relkitElemBy B.takeFill

-- | Extract end-side elements from collection.
--
--   >>> elem-end /list -to /a /b /c

consElemEnd :: (Ord c, D.CContent c) => C.RopCons c
consElemEnd med =
  do coll <- Op.getTerm  med "-coll"
     to   <- Op.getTerms med "-to"
     Right $ relmapElemEnd med (coll, to)

-- | Create @elem-end@ relmap.
relmapElemEnd :: (Ord c, D.CContent c) => C.Intmed c -> (S.TermName, [S.TermName]) -> C.Relmap c
relmapElemEnd med = C.relmapFlow med . relkitElemBy B.takeTailFill

-- | Create @elem-begin@ or @elem-end@ relkit.
relkitElemBy :: (Ord c, D.CContent c) => (c -> Int -> [c] -> [c]) -> (S.TermName, [S.TermName]) -> C.RelkitFlow c
relkitElemBy _ _ Nothing = Right C.relkitNothing
relkitElemBy f (coll, to) (Just he1) = kit2 where
    kit2 | S.termsPN [colli] toi = Right $ C.relkitJust he2 $ C.RelkitOneToOne False kitf2
         | otherwise = Msg.unkTerm [coll] he1

    [colli]  = headIndex he1 [coll]
    toi      = headIndex he1 to
    he2      = D.headAppend to he1
    kitf2 cs = let [collc] = [colli] `B.snipFrom` cs
               in f D.empty (length to) (list collc) ++ cs
    list c | D.isSet  c   = D.gSetSort c
           | D.isList c   = D.gList c
           | otherwise    = []


-- ----------------------  uncollect

consUncollect :: (Ord c, D.CSet c, D.CList c, D.CText c, D.CDec c, D.CEmpty c) => C.RopCons c
consUncollect med =
  do coll  <- Op.getTerm  med "-coll"
     to    <- Op.getTerms med "-to"
     Right $ relmapUncollect med (coll, to)

-- | Create @uncollect@ relmap.
relmapUncollect :: (Ord c, D.CSet c, D.CList c, D.CText c, D.CDec c, D.CEmpty c)
  => C.Intmed c -> (S.TermName, [S.TermName]) -> C.Relmap c
relmapUncollect med = C.relmapFlow med . relkitUncollect

-- | Create @uncollect@ relkit.
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


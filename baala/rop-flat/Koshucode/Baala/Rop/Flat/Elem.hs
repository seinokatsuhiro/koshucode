{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Operators on collection elements.

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

import qualified Koshucode.Baala.DataPlus           as K
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Rop
import qualified Koshucode.Baala.Rop.Flat.Message   as Msg

-- | Implementation of relational operators.
ropsElem :: (K.CContent c) => [C.Rop c]
ropsElem = Rop.rops "elem"
    [ consElem          K.& [ "elem /P -to /N"            K.& "-coll . -to" ]
    , consElemBegin     K.& [ "elem-begin /P -to /N ..."  K.& "-coll . -to" ]
    , consElemEnd       K.& [ "elem-end /P -to /N ..."    K.& "-coll . -to" ]
    , consIndexElem 1   K.& [ "ix-elem /P -to /N /N"      K.& "-coll . -to" ]
    , consIndexElem 0   K.& [ "iz-elem /P -to /N /N"      K.& "-coll . -to" ]
    , consMember        K.& [ "member /N /N"              K.& "-elem -set" ]
    , consUncollect     K.& [ "uncollect /P -to /N ..."   K.& "-coll . -to" ]
    , consUnroll        K.& [ "unroll /N /N -from /P ..." K.& "-term -content . -from" ]
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

consElem :: (Ord c, K.CSet c, K.CList c, K.CText c) => C.RopCons c
consElem med =
  do xs   <- Rop.getTerm med "-coll"
     x    <- Rop.getTerm med "-to"
     Right $ relmapMember med (x, xs)

-- | Expand elements from collection.
consMember :: (Ord c, K.CSet c, K.CList c, K.CText c) => C.RopCons c
consMember med =
  do x    <- Rop.getTerm med "-elem"
     xs   <- Rop.getTerm med "-set"
     Right $ relmapMember med (x, xs)

-- | Create @member@ relmap.
relmapMember :: (Ord c, K.CSet c, K.CList c, K.CText c)
  => C.Intmed c -> K.TermName2 -> C.Relmap c
relmapMember med = C.relmapFlow med . relkitMember

-- | Create @member@ relkit.
relkitMember :: (Ord c, K.CSet c, K.CList c, K.CText c)
  => K.TermName2 -> C.RelkitFlow c
relkitMember _ Nothing = Right C.relkitNothing
relkitMember (x, xs) he1'@(Just he1) = kit2 where
    kit2 | K.termsPN [xi, xsi] []   = relkitMemberCheck  xi xsi he1'
         | K.termsPN [xsi]     [xi] = relkitMemberExpand x  xsi he1'
         | otherwise                = Msg.unkTerm [x, xs] he1
    [xi, xsi] = headIndex he1 [x, xs]

relkitMemberCheck :: (Eq c, K.CSet c, K.CList c)
  => Int -> Int -> C.RelkitFlow c
relkitMemberCheck xi xsi he1' = Right kit2 where
    kit2 = C.relkit he1' $ C.RelkitTest kitf2
    kitf2 cs = let [xc, xsc] = [xi, xsi] `K.selectElems` cs
               in xc `K.isMember` xsc

relkitMemberExpand :: (Ord c, K.CSet c, K.CList c, K.CText c)
  => K.TermName -> Int -> C.RelkitFlow c
relkitMemberExpand _ _ Nothing = Right C.relkitNothing
relkitMemberExpand x xsi (Just he1) = Right kit2 where
    he2      = K.headCons x he1
    kit2     = C.relkitJust he2 $ C.RelkitMany False kitf2
    kitf2 cs = let [xsc] = [xsi] `K.selectElems` cs
               in case xsc of
                    _ | K.isSet  xsc -> map (: cs) $ K.gSet xsc
                    _ | K.isList xsc -> map (: cs) $ K.unique $ K.gList xsc
                    _ | K.isText xsc -> map (: cs) $ map (K.pText . K.list1)
                                                   $ K.unique $ K.gText xsc
                    _                -> [xsc : cs]


-- ----------------------  index-elem

-- | Expand index and element from collection.
--
--   >>> ix-elem /list -to /i /elem
--   >>> iz-elem /list -to /i /elem
--
consIndexElem :: (Ord c, K.CContent c) => Int -> C.RopCons c
consIndexElem from med =
  do xs     <- Rop.getTerm  med "-coll"
     (i, x) <- Rop.getTerm2 med "-to"
     Right $ relmapIndexElem from med (i, x, xs)

-- | Create @ix-elem@ or @iz-elem@ relmap.
relmapIndexElem :: (Ord c, K.CContent c) => Int -> C.Intmed c -> K.TermName3 -> C.Relmap c
relmapIndexElem from med = C.relmapFlow med . relkitIndexElem from

-- | Create @ix-elem@ or @iz-elem@ relkit.
relkitIndexElem :: (Ord c, K.CContent c) => Int -> K.TermName3 -> C.RelkitFlow c
relkitIndexElem _ _ Nothing = Right C.relkitNothing
relkitIndexElem from (i, x, xs) he1'@(Just he1) = kit2 where
    kit2 | K.termsPN [xsi] [xi, ii]  = relkitIndexElemExpand from i x xsi he1'
         | otherwise                 = Msg.unkTerm [i, x, xs] he1
    [ii, xi, xsi] = headIndex he1 [i, x, xs]

headIndex :: K.Head -> [K.TermName] -> [Int]
headIndex he ns = ns `K.selectIndexFull` K.getTermNames he

relkitIndexElemExpand :: forall c. (Ord c, K.CContent c)
  => Int -> K.TermName -> K.TermName -> Int -> C.RelkitFlow c
relkitIndexElemExpand _ _ _ _ Nothing = Right C.relkitNothing
relkitIndexElemExpand from i x xsi (Just he1) = Right kit2 where
    he2      = K.headAppend [i, x] he1
    kit2     = C.relkitJust he2 $ C.RelkitMany False kitf2
    kitf2 cs = let [xsc] = [xsi] `K.selectElems` cs
               in case xsc of
                    _ | K.isSet  xsc -> indexElem cs $ K.sort $ K.gSet xsc
                    _ | K.isList xsc -> indexElem cs $ K.gList xsc
                    _ | K.isText xsc -> indexElem cs $ map (K.pText . K.list1)
                                                     $ K.gText xsc
                    _                -> [xsc : cs]

    indexElem :: [c] -> [c] -> [[c]]
    indexElem cs = map (cons cs) . index

    index :: [c] -> [(c, c)]
    index = zip $ map K.pInt [from ..]

    cons :: [c] -> (c, c) -> [c]
    cons cs (ic, xc) = ic : xc : cs


-- ----------------------  unroll

-- | Unroll (expand) terms to tuples.
--
--   >>> unroll /term /content -from /1 /2 /3 /4

consUnroll :: (K.CTerm c) => C.RopCons c
consUnroll med =
  do t   <- Rop.getTerm  med "-term"
     c   <- Rop.getTerm  med "-content"
     xs  <- Rop.getTerms med "-from"
     Right $ relmapUnroll med (t, c, xs)

-- | Create @unroll@ relmap.
relmapUnroll :: (K.CTerm c) => C.Intmed c -> (K.TermName, K.TermName, [K.TermName]) -> C.Relmap c
relmapUnroll med = C.relmapFlow med . relkitUnroll

-- | Create @unroll@ relkit.
relkitUnroll :: (K.CTerm c) =>  (K.TermName, K.TermName, [K.TermName]) -> C.RelkitFlow c
relkitUnroll _ Nothing = Right C.relkitNothing
relkitUnroll (t, c, from) (Just he1) = kit2 where
    kit2 | K.termsPN fromi [ti, ci]  = Right $ C.relkitJust he2 $ C.RelkitMany False kitf2
         | otherwise                 = Msg.unkTerm (t : c : from) he1
    [ti, ci] = headIndex he1 [t, c]
    fromi    = headIndex he1 from
    he2      = K.headAppend [t, c] $ K.headMap (K.selectOthers fromi) he1
    kitf2 cs = let (fromc, cs') = fromi `K.selectBoth` cs
                   cons (term, cont) = K.pTerm term : cont : cs'
               in cons <$> zip from fromc


-- ----------------------  elem-begin

-- | Extract elements from collection.
--
--   >>> elem-begin /list -to /a /b /c

consElemBegin :: (Ord c, K.CContent c) => C.RopCons c
consElemBegin med =
  do coll <- Rop.getTerm  med "-coll"
     to   <- Rop.getTerms med "-to"
     Right $ relmapElemBegin med (coll, to)

-- | Create @elem-begin@ relmap.
relmapElemBegin :: (Ord c, K.CContent c) => C.Intmed c -> (K.TermName, [K.TermName]) -> C.Relmap c
relmapElemBegin med = C.relmapFlow med . relkitElemBy K.takeFill

-- | Extract end-side elements from collection.
--
--   >>> elem-end /list -to /a /b /c

consElemEnd :: (Ord c, K.CContent c) => C.RopCons c
consElemEnd med =
  do coll <- Rop.getTerm  med "-coll"
     to   <- Rop.getTerms med "-to"
     Right $ relmapElemEnd med (coll, to)

-- | Create @elem-end@ relmap.
relmapElemEnd :: (Ord c, K.CContent c) => C.Intmed c -> (K.TermName, [K.TermName]) -> C.Relmap c
relmapElemEnd med = C.relmapFlow med . relkitElemBy K.takeTailFill

-- | Create @elem-begin@ or @elem-end@ relkit.
relkitElemBy :: (Ord c, K.CContent c) => (c -> Int -> [c] -> [c]) -> (K.TermName, [K.TermName]) -> C.RelkitFlow c
relkitElemBy _ _ Nothing = Right C.relkitNothing
relkitElemBy f (coll, to) (Just he1) = kit2 where
    kit2 | K.termsPN [colli] toi = Right $ C.relkitLinear he2 False f'
         | otherwise = Msg.unkTerm [coll] he1

    [colli]  = headIndex he1 [coll]
    toi      = headIndex he1 to
    he2      = K.headAppend to he1
    f' cs    = let [collc] = [colli] `K.selectElems` cs
               in f K.empty (length to) (list collc) ++ cs
    list c | K.isSet  c   = K.gSetSort c
           | K.isList c   = K.gList c
           | otherwise    = []


-- ----------------------  uncollect

-- | __uncollect \/P -to \/N ...__
consUncollect :: (Ord c, K.CSet c, K.CList c, K.CText c, K.CDec c, K.CEmpty c) => C.RopCons c
consUncollect med =
  do coll  <- Rop.getTerm  med "-coll"
     to    <- Rop.getTerms med "-to"
     Right $ relmapUncollect med (coll, to)

-- | Create @uncollect@ relmap.
relmapUncollect :: (Ord c, K.CSet c, K.CList c, K.CText c, K.CDec c, K.CEmpty c)
  => C.Intmed c -> (K.TermName, [K.TermName]) -> C.Relmap c
relmapUncollect med = C.relmapFlow med . relkitUncollect

-- | Create @uncollect@ relkit.
relkitUncollect :: (Ord c, K.CSet c, K.CList c, K.CText c, K.CDec c, K.CEmpty c)
  => (K.TermName, [K.TermName]) -> C.RelkitFlow c
relkitUncollect _ Nothing = Right C.relkitNothing
relkitUncollect (coll, to) (Just he1) = kit2 where
    kit2 | K.termsPN icoll ito  = Right $ C.relkitLinear he2 False f
         | otherwise            = Msg.unkTerm (coll : to) he1

    icoll    = headIndex he1 [coll]
    ito      = headIndex he1 to
    he2      = K.headAppend to he1
    f cs     = let [xsc]    = icoll `K.selectElems` cs
                   char     = K.pText . K.list1
                   ys << xs = appendCount K.empty (length to) xs ys
               in case () of
                    _ | K.isSet  xsc  -> cs << (K.sort $ K.gSet xsc)
                      | K.isList xsc  -> cs << (K.gList xsc)
                      | K.isText xsc  -> cs << (map char $ K.gText xsc)
                      | otherwise     -> cs << []

appendCount :: a -> Int -> [a] -> [a] -> [a]
appendCount fill num xs ys = loop num xs where
    loop 0 _        = ys
    loop n (x:xs2)  = x    : loop (n - 1) xs2
    loop n []       = fill : loop (n - 1) []


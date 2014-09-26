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

  -- * type-name
  consTypeName,
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
    --         CONSTRUCTOR   USAGE                     ATTRIBUTE
    [ Op.ropV  consAssn      "assn /P ... -to N"       "-term | -to"
    , Op.ropE  consMember    "member /N /N"            "-1 -2"
    , Op.ropIV consRdf       "rdf P /S /O"             "-pattern -term"
    , Op.ropI  consTermName  "term-name /N"            "-term"
    , Op.ropV  consTypeName  "type-name /N /P ..."     "-term"
    , Op.ropI  consUnassn    "unassn /P -only /P ..."  "-from | -only"
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
consRdf use =
    do sign  <- Op.getWord  use "-pattern"
       [s,o] <- Op.getTerms use "-term"
       Right $ C.relmapSource use sign ["/s", "/o"] `B.mappend`
               Op.relmapRename use [(s,"/s"), (o,"/o")]



-- ----------------------  assn

--    > assn /x /y /z -to /a

consAssn :: (C.CAssn c) => C.RopCons c
consAssn use =
  do ns <- Op.getTerms use "-term"
     to <- Op.getTerm  use "-to"
     Right $ relmapAssn use (ns, to)

relmapAssn :: (C.CAssn c) => C.RopUse c -> ([B.TermName], B.TermName) -> C.Relmap c
relmapAssn use = C.relmapFlow use . relkitAssn

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
consUnassn use =
  do from <- Op.getTerm  use "-from"
     ns   <- Op.getTerms use "-only"
     Right $ relmapUnassn use (from, ns)

relmapUnassn :: (C.CAssn c) => C.RopUse c -> (B.TermName, [B.TermName]) -> C.Relmap c
relmapUnassn use = C.relmapFlow use . relkitUnassn

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
consTermName use =
  do n <- Op.getTerm use "-term"
     Right $ relmapTermName use n

relmapTermName :: (C.CTerm c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapTermName use n = C.relmapFlow use $ relkitTermName n

relkitTermName :: (C.CTerm c) => B.TermName -> C.RelkitFlow c
relkitTermName _ Nothing    = Msg.noAttr
relkitTermName n (Just he1) = Right kit2 where
    he2       = B.headFrom [n]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 _   = map term $ B.headNames he1
    term t    = [C.pTerm t]


-- ----------------------  type-name

-- | Get typename.
consTypeName :: (C.CContent c) => C.RopCons c
consTypeName use =
  do np <- Op.getTermPairs use "-term"
     Right $ C.relmapFlow use $ relkitTypename np

relkitTypename :: (C.CText c) => [B.TermName2] -> C.RelkitFlow c
relkitTypename _ Nothing = Right C.relkitNothing
relkitTypename np (Just he1) = Right kit2 where
    ns, ps :: [B.TermName]
    (ns, ps)  = unzip np

    ns1       = B.headNames he1
    ind1      = ps `B.snipIndex` ns1
    share1    = B.snipFrom ind1

    he2       = ns `B.headAppend` he1
    kit2      = C.relkitJust he2 $ C.RelkitOneToOne False kitf2
    kitf2 cs1 = let cs2 = share1 cs1 in map typetext cs2 ++ cs1
    typetext  = C.pText . C.typename


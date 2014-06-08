{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Term
( ropsTerm,
  -- * pick
  consPick, relmapPick, relkitPick,
  -- * cut
  consCut, relmapCut, relkitCut,
  -- * pick-term
  consPickTerm, relmapPickTerm, relkitPickTerm,
  -- * cut-term
  consCutTerm, relmapCutTerm, relkitCutTerm,
  -- * move
  consMove, relmapMove, relkitMove,
  -- * rename
  consRename, relmapRename, relkitRename,
  -- * prefix
  consPrefix, relmapPrefix, relkitPrefix,
  -- * unprefix
  consUnprefix, relmapUnprefix, relkitUnprefix,
  -- * prefix-change
  consPrefixChange, relmapPrefixChange, relkitPrefixChange,
) where

import qualified Data.List                  as List
import qualified Data.Maybe                 as Maybe
import qualified Data.Tuple                 as Tuple
import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Message as Message


-- | Relmap operators for manipulating term names.
--
--   [@cut@]        Project relation to unspecified terms.
--
--   [@cut-term@]   Project relation to terms not in relmap output.
--
--   [@pick@]       Project relation to specified terms.
--
--   [@pick-term@]  Project relation to terms in relmap output.
--
--   [@rename@]     Change term name.
--
--   [@move@]       Change heading.
--
--   [@prefix \/P \/N ...@]
--     Add prefix @\/P@ to terms @\/N@ ...
-- 
--   [@prefix-change \/P \/Q@]
--     Change prefix from @\/P@ to @\/Q@.
-- 
--   [@unprefix \/P@]
--     Remove prefix @\/P@ from term name.
-- 
ropsTerm :: (Ord c) => [C.Rop c]
ropsTerm = Op.ropList "term"  -- GROUP
    --   USAGE                   , CONSTRUCTOR      , ATTRIBUTE
    [ ( "cut /P ..."             , consCut          , C.roaList "-term" [] )
    , ( "cut-term /R"            , consCutTerm      , C.roaOne  "-relmap" [] )
    , ( "pick /P ..."            , consPick         , C.roaList "-term"   [] )
    , ( "pick-term /R"           , consPickTerm     , C.roaOne  "-relmap" [] )
    , ( "rename /N /P ..."       , consRename       , C.roaList "-term"   [] )
    , ( "move /P ... -to /N ..." , consMove         , C.roaList "-term" ["-to"] )
    , ( "prefix /P /N ..."       , consPrefix       , C.roaOneList "-prefix" "-term" [] )
    , ( "prefix-change /P /Q"    , consPrefixChange , C.roaTwo "-new" "-old" [] )
    , ( "unprefix /P"            , consUnprefix     , C.roaOne "-prefix" [] )
    ]


-- ----------------------  pick & cut

consPick :: C.RopCons c
consPick use =
  do ns <- Op.getTerms use "-term"
     Right $ relmapPick use ns

relmapPick :: C.RopUse c -> [B.TermName] -> C.Relmap c
relmapPick use = C.relmapFlow use . relkitPick

relkitPick :: [B.TermName] -> C.RelkitFlow c
relkitPick = relkitSnip B.snipFrom B.snipFrom

consCut :: C.RopCons c
consCut use =
  do ns <- Op.getTerms use "-term"
     Right $ relmapCut use ns

relmapCut :: C.RopUse c -> [B.TermName] -> C.Relmap c
relmapCut use = C.relmapFlow use . relkitCut

relkitCut :: [B.TermName] -> C.RelkitFlow c
relkitCut = relkitSnip B.snipOff B.snipOff


-- ----------------------  pick-term & cut-term

consPickTerm :: C.RopCons c
consPickTerm use =
  do rmap <- Op.getRelmap use
     Right $ relmapPickTerm use rmap

relmapPickTerm :: C.RopUse c -> C.Relmap c -> C.Relmap c
relmapPickTerm use = C.relmapBinary use relkitPickTerm

relkitPickTerm :: C.RelkitBinary c
relkitPickTerm = relkitSnipTerm B.snipFrom B.snipFrom

consCutTerm :: C.RopCons c
consCutTerm use =
  do rmap <- Op.getRelmap use
     Right $ relmapCutTerm use rmap

relmapCutTerm :: C.RopUse c -> C.Relmap c -> C.Relmap c
relmapCutTerm use = C.relmapBinary use relkitCutTerm

relkitCutTerm :: C.RelkitBinary c
relkitCutTerm = relkitSnipTerm B.snipOff B.snipOff


-- ----------------------  snip

relkitSnipTerm :: B.Snip B.Term -> B.Snip c -> C.RelkitBinary c
relkitSnipTerm _ _ (C.Relkit Nothing _) = const $ Right C.relkitNothing
relkitSnipTerm heSnip boSnip (C.Relkit (Just he2) _) =
    relkitSnip heSnip boSnip $ B.headNames he2

relkitSnip :: B.Snip B.Term -> B.Snip c -> [B.TermName] -> C.RelkitFlow c
relkitSnip _ _ _ Nothing = Right C.relkitNothing
relkitSnip heSnip boSnip ns (Just he1)
    | B.sameLength ns ind1 = Right kit2
    | otherwise = Message.unkTerm non he1
    where
      he2   =  B.headChange (heSnip ind1) he1
      kit2  =  C.relkitJust he2 $ C.RelkitOneToOne True $ boSnip ind1
      ns1   =  B.headNames he1
      non   =  B.snipOff ind ns
      ind1  =  ns  `B.snipIndex` ns1
      ind   =  ns1 `B.snipIndex` ns


-- ----------------------  move

consMove :: C.RopCons c
consMove use =
  do ps <- Op.getTerms use "-term"
     ns <- Op.getTerms use "-to"
     Right $ relmapMove use (ps, ns)

relmapMove :: C.RopUse c -> ([B.TermName], [B.TermName]) -> C.Relmap c
relmapMove use = C.relmapFlow use . relkitMove

relkitMove :: ([B.TermName], [B.TermName]) -> C.RelkitFlow c
relkitMove _ Nothing = Right C.relkitNothing
relkitMove (ps, ns) (Just he1)
    | B.notSameLength ps ns = Message.oddAttr
    | dup     /= []         = Message.dupTerm dup he2
    | psLeft  /= []         = Message.unkTerm psLeft he1
    | otherwise             = Right kit2
    where
      ns1      = B.headNames he1
      ns2      = B.headNames he2
      dup      = B.duplicate ns2

      psLeft   = ps `B.snipLeft`  ns1
      psInd    = ps `B.snipIndex` ns1

      he2      = B.headChange move he1
      kit2     = C.relkitJust he2 C.RelkitId

      move ts  = foldr mv ts mvlist
      mv (i,n) = B.termChange (const n) `at` i
      mvlist   = zip psInd ns

at :: (B.Map a) -> Int -> B.Map [a]
at f = loop where
    loop 0 (x : xs) = f x : xs
    loop i (x : xs) = x : loop (i - 1) xs
    loop _ []       = []



-- ----------------------  rename

consRename :: C.RopCons c
consRename use =
  do np <- Op.getTermPairs use "-term"
     Right $ relmapRename use np

relmapRename :: C.RopUse c -> [B.TermName2] -> C.Relmap c
relmapRename use = C.relmapFlow use . relkitRename

relkitRename :: [B.TermName2] -> C.RelkitFlow c
relkitRename _ Nothing = Right C.relkitNothing
relkitRename np (Just he1)
    | nsShare /= [] = Message.reqNewTerm nsShare he1
    | psLeft  /= [] = Message.unkTerm    psLeft  he1
    | otherwise     = Right kit2
    where
      (ns, ps) = unzip np
      ns1      = B.headNames he1
      nsShare  = ns `B.snipShare` ns1
      psLeft   = ps `B.snipLeft`  ns1

      pn       = map Tuple.swap np
      ren p    = Maybe.fromMaybe p $ lookup p pn

      he2      = B.headRename ren he1
      kit2     = C.relkitJust he2 C.RelkitId


-- ----------------------  prefix

consPrefix :: C.RopCons c
consPrefix use =
    do pre <- Op.getTerm  use "-prefix"
       ns  <- Op.getTerms use "-term"
       Right $ relmapPrefix use pre ns

relmapPrefix :: C.RopUse c -> String -> [String] -> C.Relmap c
relmapPrefix use pre ns = C.relmapFlow use $ relkitPrefix pre ns

-- | Add prefix to specified terms.
relkitPrefix :: String -> [String] -> C.RelkitFlow c
relkitPrefix _ _ Nothing = Right C.relkitNothing
relkitPrefix pre ns (Just he1) = Right kit2 where
    he2 =  B.headRename f he1
    kit2 = C.relkitId $ Just he2
    f n | n `elem` ns  = prefixName pre n
        | otherwise    = n

prefixName :: String -> String -> String
prefixName pre ('/' : ns) = pre ++ "-" ++ ns
prefixName _ _ = undefined



-- ----------------------  unprefix

consUnprefix :: C.RopCons c
consUnprefix use =
    do pre <- Op.getTerm use "-prefix"
       Right $ relmapUnprefix use pre

relmapUnprefix :: C.RopUse c -> String -> C.Relmap c
relmapUnprefix use = C.relmapFlow use . relkitUnprefix

-- | Remove prefix
relkitUnprefix :: String -> C.RelkitFlow c
relkitUnprefix _ Nothing = Right C.relkitNothing
relkitUnprefix pre (Just he1) = Right kit2 where
    he2  = B.headRename (unprefixName pre) he1
    kit2 = C.relkitId $ Just he2

unprefixName :: String -> String -> String
unprefixName pre n =
    case List.stripPrefix pre n of
      Just ('-' : n2) -> '/' : n2
      _ -> n



-- ----------------------  prefix-change

consPrefixChange :: C.RopCons c
consPrefixChange use =
    do new <- Op.getTerm use "-new"
       old <- Op.getTerm use "-old"
       Right $ relmapPrefixChange use (new, old)

relmapPrefixChange :: C.RopUse c -> (String, String) -> C.Relmap c
relmapPrefixChange use = C.relmapFlow use . relkitPrefixChange

-- | Change prefix
relkitPrefixChange :: (String, String) -> C.RelkitFlow c
relkitPrefixChange _ Nothing = Right C.relkitNothing
relkitPrefixChange (new, old) (Just he1) = Right kit2 where
    he2  = B.headRename f he1
    kit2 = C.relkitId $ Just he2
    new' = new ++ "-"
    old' = old ++ "-"
    f n' = case List.stripPrefix old' n' of
             Just n2 -> new' ++ n2
             Nothing -> n'


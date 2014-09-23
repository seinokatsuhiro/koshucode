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
  consRename, relmapRename,
  -- * prefix
  consPrefix, relmapPrefix, relkitPrefix,
  -- * unprefix
  consUnprefix, relmapUnprefix, relkitUnprefix,
  -- * prefix-change
  consPrefixChange, relmapPrefixChange, relkitPrefixChange,
) where

import qualified Data.List                  as List
import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Message as Msg


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
    --          CONSTRUCTOR        USAGE                      ATTRIBUTE
    [ Op.ropV   consCut            "cut /P ..."               "-term"
    , Op.ropI   consCutTerm        "cut-term /R"              "-relmap/"
    , Op.ropV   consPick           "pick /P ..."              "-term"
    , Op.ropI   consPickTerm       "pick-term /R"             "-relmap/"
    , Op.ropV   consRename         "rename /N /P ..."         "-term"
    , Op.ropV   consMove           "move /P ... -to /N ..."   "-from | -to"
    , Op.ropIV  consPrefix         "prefix /P /N ..."         "-prefix -term"
    , Op.ropII  consPrefixChange   "prefix-change /P /Q"      "-new -old"
    , Op.ropI   consUnprefix       "unprefix /P"              "-prefix"
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
  do rmap <- Op.getRelmap use "-relmap"
     Right $ relmapPickTerm use rmap

relmapPickTerm :: C.RopUse c -> C.Relmap c -> C.Relmap c
relmapPickTerm use = C.relmapBinary use relkitPickTerm

relkitPickTerm :: C.RelkitBinary c
relkitPickTerm = relkitSnipTerm B.snipFrom B.snipFrom

consCutTerm :: C.RopCons c
consCutTerm use =
  do rmap <- Op.getRelmap use "-relmap"
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
    | otherwise = Msg.unkTerm non he1
    where
      he2   =  B.headChange (heSnip ind1) he1
      kit2  =  C.relkitJust he2 $ C.RelkitOneToOne True $ boSnip ind1
      ns1   =  B.headNames he1
      non   =  B.snipOff ind ns
      ind1  =  ns  `B.snipIndex` ns1
      ind   =  ns1 `B.snipIndex` ns


-- ----------------------  rename

consRename :: C.RopCons c
consRename use =
  do np <- Op.getTermPairs use "-term"
     Right $ relmapRename use np

relmapRename :: C.RopUse c -> [B.TermName2] -> C.Relmap c
relmapRename use = C.relmapFlow use . relkitMove . unzip . map B.swap


-- ----------------------  move

consMove :: C.RopCons c
consMove use =
  do ps <- Op.getTerms use "-from"
     ns <- Op.getTerms use "-to"
     Right $ relmapMove use (ps, ns)

relmapMove :: C.RopUse c -> ([B.TermName], [B.TermName]) -> C.Relmap c
relmapMove use = C.relmapFlow use . relkitMove

relkitMove :: ([B.TermName], [B.TermName]) -> C.RelkitFlow c
relkitMove _ Nothing = Right C.relkitNothing
relkitMove (ps, ns) (Just he1)
    | B.notSameLength ps ns = Msg.oddAttr
    | psDup   /= []         = Msg.dupTerm psDup he1
    | ns2Dup  /= []         = Msg.dupTerm ns2Dup he2
    | psLeft  /= []         = Msg.unkTerm psLeft he1
    | otherwise             = Right kit2
    where
      ns1        =  B.headNames he1
      ns2        =  B.headNames he2

      ns2Dup     =  B.duplicates ns2
      psDup      =  B.duplicates ps

      psLeft     =  ps `B.snipLeft`  ns1
      psIndex    =  ps `B.snipIndex` ns1

      he2        =  B.headChange mvAll he1
      kit2       =  C.relkitJust he2 C.RelkitId

      mvAll :: B.Map [B.Term]
      mvAll ts   =  foldr mv ts $ zip psIndex ns

      mv :: (Int, B.TermName) -> B.Map [B.Term]
      mv (i, n)  =  B.termChange (const n) `B.mapAt` i


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


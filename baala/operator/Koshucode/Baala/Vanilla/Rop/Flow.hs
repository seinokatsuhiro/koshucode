{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Flow
( 
  -- * enclose
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

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Builtin       as Rop
import qualified Koshucode.Baala.Minimal       as Rop
import qualified Koshucode.Baala.Vanilla.Type  as Rop



-- ----------------------  enclose

consEnclose :: (C.CRel c) => C.RopCons c
consEnclose use =
  do n <- Rop.getTerm use "-term"
     Right $ relmapEnclose use n

relmapEnclose :: (C.CRel c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapEnclose use = C.relmapFlow use . relkitEnclose

{-| Enclose the current relation in a term. -}
relkitEnclose :: (C.CRel c) => B.Termname -> C.RelkitCalc c
relkitEnclose n h1 = Right $ C.relkit h2 (C.RelkitFull False f) where
    h2 = B.Relhead [B.Nest n $ B.headTerms h1]
    f b1 = [[C.pRel $ B.Rel h1 b1]]



-- ----------------------  member

{- $member

   Membership.
   Relmap @member@ @\/x@ @\/xs@ means
   @\/x@ is a member of @\/xs@.

   - Input relation has both @\/x@ and @\/xs@,
     check content of @\/x@ is acturally the member of @\/xs@.

   - Input relation has @\/xs@ and not @\/x@,
     add term @\/x@ as member of @\/xs@.
-}  

consMember :: Rop.VRopCons
consMember use =
  do x    <- Rop.getTerm use "-1"
     xs   <- Rop.getTerm use "-2"
     Right $ relmapMember use (x, xs)

relmapMember :: C.RopUse Rop.VContent -> B.Termname2 -> C.Relmap Rop.VContent
relmapMember use = C.relmapFlow use . relkitMember

relkitMember :: B.Termname2 -> C.RelkitCalc Rop.VContent
relkitMember (x, xs) h1 = r2 where
    r2 | xHere && xsHere     = relkitMemberCheck  xPos xsPos h1
       | not xHere && xsHere = relkitMemberExpand x    xsPos h1
       | otherwise           = Left $ B.AbortAnalysis [] (B.AANoTerms [x, xs])
    ([xPos, xsPos], [xHere, xsHere])
        = h1 `B.posHere` [x, xs]

relkitMemberCheck :: B.TermPos -> B.TermPos -> C.RelkitCalc Rop.VContent
relkitMemberCheck xPos xsPos h1 = Right $ C.relkit h1 (C.RelkitPred f) where
    f cs = let [xCont, xsCont] = B.posPick [xPos, xsPos] cs
           in xCont `Rop.isMember` xsCont

relkitMemberExpand :: B.Termname -> B.TermPos -> C.RelkitCalc Rop.VContent
relkitMemberExpand x xsPos h1 = Right $ C.relkit h2 (C.RelkitOneToMany False f) where
    h2     =  B.headCons x h1
    f cs   =  let [xsCont] = B.posPick [xsPos] cs
              in case xsCont of
                   Rop.VSet  xs -> map (: cs) xs
                   Rop.VList xs -> map (: cs) $ B.unique xs
                   _            -> [xsCont : cs]



-- ----------------------  range

consRange :: (C.CDec c) => C.RopCons c
consRange use =
  do term <- Rop.getTerm use "-term"
     low  <- Rop.getInt  use "-from"
     high <- Rop.getInt  use "-to"
     Right $ relmapRange use term low high

relmapRange :: (C.CDec c) => C.RopUse c -> B.Termname -> Int -> Int -> C.Relmap c
relmapRange use term low high = C.relmapFlow use $ relkitRange term low high

relkitRange :: (C.CDec c) => B.Termname -> Int -> Int -> C.RelkitCalc c
relkitRange n low high h1 = Right $ C.relkit h2 (C.RelkitOneToMany False f) where
    h2    = B.headCons n h1
    decs  = map C.pDecFromInt [low .. high]
    f cs  = map (: cs) decs



-- ----------------------  RDF

consRdf :: C.RopCons c
consRdf use =
    do sign  <- Rop.getWord  use "-pattern"
       [s,o] <- Rop.getTerms use "-term"
       Right $ C.relmapAlias use $
             C.relmapSource use sign ["/s", "/o"] `B.mappend`
             Rop.relmapRename use [(s,"/s"), (o,"/o")]



-- ----------------------  size

consSize :: (C.CDec c) => C.RopCons c
consSize use =
  do n <- Rop.getTerm use "-term"
     Right $ relmapSize use n

relmapSize :: (C.CDec c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapSize use n = C.relmapFlow use $ relkitSize n

{-| Cardinality -}
relkitSize :: (C.CDec c) => B.Termname -> C.RelkitCalc c
relkitSize n _ = Right $ C.relkit h2 (C.RelkitFull False f) where
    h2   = B.headFrom [n]
    f b1 = [[C.pDecFromInt $ length b1]]


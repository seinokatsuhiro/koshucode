{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Calc
( 
  -- * enclose
  ropConsEnclose, relmapEnclose, relfyEnclose,
  -- * member
  -- $member
  ropConsMember, relmapMember, relfyMember,
  -- * range
  ropConsRange, relmapRange,
  -- * RDF
  ropConsRdf,
  -- * size
  ropConsSize, relmapSize, relfySize,
) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Builtin       as Rop
import qualified Koshucode.Baala.Minimal       as Rop
import qualified Koshucode.Baala.Vanilla.Type  as Rop



-- ----------------------  enclose

ropConsEnclose :: (C.CRel c) => C.RopCons c
ropConsEnclose use =
  do n <- Rop.getTerm use "-term"
     Right $ relmapEnclose use n

relmapEnclose :: (C.CRel c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapEnclose use n = C.relmapCalc use $ relfyEnclose n

{-| Enclose the current relation in a term. -}
relfyEnclose
    :: (C.CRel c)
    => B.Termname         -- ^ Termname of enclosed relation
    -> B.Relhead          -- ^ Header of input relation
    -> B.Ab (C.Relfy c)   -- ^ Relfier for output relation
relfyEnclose n h1 = Right $ C.relfy h2 (C.RelfyFull False f) where
    h2 = B.Relhead [B.Nest n $ B.headTerms h1]
    f b1 = [[C.putRel $ B.Rel h1 b1]]



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

ropConsMember :: Rop.VRopCons
ropConsMember use =
  do x    <- Rop.getTerm use "-1"
     xs   <- Rop.getTerm use "-2"
     Right $ relmapMember use x xs

relmapMember :: C.RopUse Rop.VContent -> B.Termname -> B.Termname -> C.Relmap Rop.VContent
relmapMember use x xs = C.relmapCalc use $ relfyMember x xs

relfyMember :: B.Termname -> B.Termname -> B.Relhead -> B.Ab (C.Relfy Rop.VContent)
relfyMember x xs h1 = r2 where
    r2 | xHere && xsHere     = relfyMemberCheck  xPos xsPos h1
       | not xHere && xsHere = relfyMemberExpand x    xsPos h1
       | otherwise           = Left $ B.AbortAnalysis [] (B.AANoTerms [x, xs])
    ([xPos, xsPos], [xHere, xsHere])
        = h1 `B.posHere` [x, xs]

relfyMemberCheck :: B.TermPos -> B.TermPos -> B.Relhead -> B.Ab (C.Relfy Rop.VContent)
relfyMemberCheck xPos xsPos h1 = Right $ C.relfy h1 (C.RelfyPred f) where
    f cs = let [xCont, xsCont] = B.posPick [xPos, xsPos] cs
           in xCont `Rop.isMember` xsCont

relfyMemberExpand :: B.Termname -> B.TermPos -> B.Relhead -> B.Ab (C.Relfy Rop.VContent)
relfyMemberExpand x xsPos h1 = Right $ C.relfy h2 (C.RelfyOneToMany False f) where
    h2     =  B.headCons x h1
    f cs   =  let [xsCont] = B.posPick [xsPos] cs
              in case xsCont of
                   Rop.VSet  xs -> map (: cs) xs
                   Rop.VList xs -> map (: cs) $ B.unique xs
                   _            -> [xsCont : cs]



-- ----------------------  range

ropConsRange :: (C.CDec c) => C.RopCons c
ropConsRange use =
  do term <- Rop.getTerm use "-term"
     low  <- Rop.getInt  use "-from"
     high <- Rop.getInt  use "-to"
     Right $ relmapRange use term low high

relmapRange :: (C.CDec c) => C.RopUse c -> B.Termname -> Int -> Int -> C.Relmap c
relmapRange use term low high = C.relmapCalc use $ relfyRange term low high

relfyRange
  :: (C.CDec c) =>
     B.Termname -> Int -> Int -> B.Relhead -> B.Ab (C.Relfy c)
relfyRange n low high h1 = Right $ C.relfy h2 (C.RelfyOneToMany False f) where
    h2    = B.headCons n h1
    decs  = map C.putDecFromInt [low .. high]
    f cs  = map (: cs) decs



-- ----------------------  RDF

ropConsRdf :: C.RopCons c
ropConsRdf use =
    do sign  <- Rop.getWord  use "-pattern"
       [s,o] <- Rop.getTerms use "-term"
       Right $ C.relmapAlias use $
             C.relmapSource use sign ["/s", "/o"] `B.mappend`
             Rop.relmapRename use [(s,"/s"), (o,"/o")]



-- ----------------------  size

ropConsSize :: (C.CDec c) => C.RopCons c
ropConsSize use =
  do n <- Rop.getTerm use "-term"
     Right $ relmapSize use n

relmapSize :: (C.CDec c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapSize use n = C.relmapCalc use $ relfySize n

{-| Cardinality -}
relfySize
    :: (C.CDec c)
    => B.Termname         -- ^ Name of new term
    -> B.Relhead
    -> B.Ab (C.Relfy c)   -- ^ Relfier for output relation
relfySize n _ = Right $ C.relfy h2 (C.RelfyFull False f) where
    h2   = B.headFrom [n]
    f b1 = [[C.putDecFromInt $ length b1]]




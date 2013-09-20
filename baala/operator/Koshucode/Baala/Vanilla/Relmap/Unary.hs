{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Unary
( 
  -- * size
  ropConsSize, relmapSize, relfySize,
  -- * enclose
  ropConsEnclose, relmapEnclose, relfyEnclose,
  -- * rank
  ropConsRank, relmapRank, relfyRank, -- limit,
  -- * typename
  ropConsTypename, relmapTypename, relfyTypename,
  -- * range
  ropConsRange, relmapRange,
  -- * duplicate
  -- $duplicate
  ropConsDuplicate, relmapDuplicate,
  -- * member
  -- $member
  ropConsMember, relmapMember, relfyMember,
  -- * RDF
  ropConsRdf
) where

import qualified Data.Map as Map
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin
import qualified Koshucode.Baala.Minimal as Mini
import Koshucode.Baala.Vanilla.Type
import Koshucode.Baala.Vanilla.Order



-- ----------------------  size

ropConsSize :: C.RopCons VContent
ropConsSize use =
  do n <- Builtin.getTerm use "-term"
     Right $ relmapSize use n

relmapSize :: (C.CDec c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapSize use n = C.relmapCalc use "size" fy where
    fy _ _ = relfySize n

{-| Cardinality -}
relfySize
    :: (C.CDec c)
    => B.Termname         -- ^ Name of new term
    -> B.Ab (C.Relfy c)   -- ^ Relfier for output relation
relfySize n = Right $ C.Relfy h2 (C.RelfyFull False f) where
    h2   = B.headFrom [n]
    f b1 = [[C.putDecFromInt $ length b1]]



-- ----------------------  enclose

ropConsEnclose :: C.RopCons VContent
ropConsEnclose use =
  do n <- Builtin.getTerm use "-term"
     Right $ relmapEnclose use n

relmapEnclose :: (C.CRel c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapEnclose use n = C.relmapCalc use "enclose" fy where
    fy _ = relfyEnclose n

{-| Enclose the current relation in a term. -}
relfyEnclose
    :: (C.CRel c)
    => B.Termname         -- ^ Termname of enclosed relation
    -> B.Relhead          -- ^ Header of input relation
    -> B.Ab (C.Relfy c)   -- ^ Relfier for output relation
relfyEnclose n h1 = Right $ C.Relfy h2 (C.RelfyFull False f) where
    h2 = B.Relhead [B.Nest n $ B.headTerms h1]
    f b1 = [[C.putRel $ B.Rel h1 b1]]



-- ----------------------  rank

ropConsRank :: C.RopCons VContent
ropConsRank use =
    do n  <- Builtin.getTerm  use "-add"
       ns <- Builtin.getTerms use "-order"
       Right $ relmapRank use n ns

relmapRank :: (C.CDec c, Ord c) => C.RopUse c -> B.Termname -> [B.Termname] -> C.Relmap c
relmapRank use n ns = C.relmapCalc use "rank" fy where
    fy _ = relfyRank n ns

relfyRank
    :: (Ord c, C.CDec c)
    => B.Termname         -- ^ Name of rank term
    -> [B.Termname]       -- ^ Termnames for order
    -> B.Relhead          -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)   -- ^ Relfier for output relation
relfyRank n ns h1 = Right $ C.Relfy h2 (C.RelfyFull False f2) where
    h2    = B.headCons n h1
    f2 b1 = let b1' = sortByName ords (B.headNames h1) b1
            in zipWith (:) (map C.putDecFromInt [1..]) b1'
    ords  = map Asc ns

-- -- | Keep leading tuples.
-- limit :: (Ord c) => C.RopUse c -> Int -> String -> C.Relmap c
-- limit use c ns = C.relmapCalc use "limit" (limit2 c ns)

-- limit2 :: (Ord c) => Int -> String -> a -> B.AbMap (B.Rel c)
-- limit2 c ns _ (B.Rel h1 b1) = Right $ B.Rel h1 b2 where
--     b2   = List.take c $ sortByName ords (B.headNames h1) b1
--     ords = orders ns



-- ----------------------  typename

ropConsTypename :: (C.CContent c) => C.RopCons c
ropConsTypename use = do
  (n, p) <- Builtin.getTermPair use "-term"
  Right $ relmapTypename use n p

relmapTypename :: (C.CContent c) => C.RopUse c -> B.Termname -> B.Termname -> C.Relmap c
relmapTypename use n p = C.relmapCalc use "typename" fy where
    fy _ = relfyTypename n p

{-| Get typename. -}
relfyTypename
  :: (C.CText c) =>
     B.Termname -> B.Termname -> B.Relhead -> B.Ab (C.Relfy c)
relfyTypename n p h1 = Right $ C.Relfy h2 (C.RelfyOneToOne False f) where
    h2    = B.headCons n h1
    pos   = h1 `B.posFor` [p]
    f cs1 = let [c] = B.posPick pos cs1
            in C.putText (C.typename c) : cs1



-- ----------------------  range

ropConsRange :: C.RopCons VContent
ropConsRange use =
  do term <- Builtin.getTerm use "-term"
     low  <- Builtin.getInt  use "-from"
     high <- Builtin.getInt  use "-to"
     Right $ relmapRange use term low high

relmapRange :: (C.CDec c) => C.RopUse c -> B.Termname -> Int -> Int -> C.Relmap c
relmapRange use term low high = C.relmapCalc use "range" fy where
    fy _ = relfyRange term low high

relfyRange
  :: (C.CDec c) =>
     B.Termname -> Int -> Int -> B.Relhead -> B.Ab (C.Relfy c)
relfyRange n low high h1 = Right $ C.Relfy h2 (C.RelfyOneToMany False f) where
    h2    = B.headCons n h1
    decs  = map C.putDecFromInt [low .. high]
    f cs  = map (: cs) decs



-- ----------------------  duplicate

{- $duplicate

   Output tuples of which key is duplicated.
   Relmap @duplicate@ @\/x@ @\/y@ means
   if set of terms @\/x@ and @\/y@ is a key of relation,
   there are another tuples that has the same key.

-}  

ropConsDuplicate :: (Ord c) => C.RopCons c
ropConsDuplicate use =
  do ns <- Builtin.getTerms use "-term"
     Right $ relmapDuplicate use ns

relmapDuplicate :: (Ord c) => C.RopUse c -> [B.Termname] -> C.Relmap c
relmapDuplicate use ns = C.relmapCalc use "duplicate" fy where
    fy _ = relfyDuplicate ns

relfyDuplicate
  :: (Ord c) => [B.Termname] -> B.Relhead -> B.Ab (C.Relfy c)
relfyDuplicate ns h1
    | null non  = Right $ C.Relfy h1 (C.RelfyFull False f)
    | otherwise = Left  $ B.AbortNoTerms non
    where
      non :: [B.Termname]
      non = B.headNonExistTerms h1 ns

      pos :: [B.TermPos]
      pos = h1 `B.posFor` ns

      f :: (Ord c) => [[c]] -> [[c]]
      f b1    = let m = B.gatherToMap $ map kv b1
                in concat $ Map.elems $ Map.filter dup m
      kv cs1  = ( B.posPick pos cs1, cs1 )
      dup     = not . B.isSingleton



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

ropConsMember :: C.RopCons VContent
ropConsMember use =
  do x    <- Builtin.getTerm use "-1"
     xs   <- Builtin.getTerm use "-2"
     Right $ relmapMember use x xs

relmapMember :: C.RopUse VContent -> B.Termname -> B.Termname -> C.Relmap VContent
relmapMember use x xs = C.relmapCalc use "member" fy where
    fy _ = relfyMember x xs

relfyMember :: B.Termname -> B.Termname -> B.Relhead -> B.Ab (C.Relfy VContent)
relfyMember x xs h1 = r2 where
    r2 | xHere && xsHere     = relfyMemberCheck  xPos xsPos h1
       | not xHere && xsHere = relfyMemberExpand x    xsPos h1
       | otherwise           = Left $ B.AbortNoTerms [x, xs]
    ([xPos, xsPos], [xHere, xsHere])
        = h1 `B.posHere` [x, xs]

relfyMemberCheck :: B.TermPos -> B.TermPos -> B.Relhead -> B.Ab (C.Relfy VContent)
relfyMemberCheck xPos xsPos h1 = Right $ C.Relfy h1 (C.RelfyPred f) where
    f cs = let [xCont, xsCont] = B.posPick [xPos, xsPos] cs
           in xCont `isMember` xsCont

relfyMemberExpand :: B.Termname -> B.TermPos -> B.Relhead -> B.Ab (C.Relfy VContent)
relfyMemberExpand x xsPos h1 = Right $ C.Relfy h2 (C.RelfyOneToMany False f) where
    h2     =  B.headCons x h1
    f cs   =  let [xsCont] = B.posPick [xsPos] cs
              in case xsCont of
                   VSet  xs  -> map (: cs) xs
                   VList xs  -> map (: cs) $ B.unique xs
                   _         -> [xsCont : cs]



-- ----------------------  RDF

ropConsRdf :: C.RopCons VContent
ropConsRdf use =
    do sign  <- Builtin.getWord  use "-sign"
       [s,o] <- Builtin.getTerms use "-term"
       Right $ C.relmapAlias use $
             C.relmapSource use sign ["/s", "/o"] `Builtin.mappend`
             Mini.relmapRename use [(s,"/s"), (o,"/o")]


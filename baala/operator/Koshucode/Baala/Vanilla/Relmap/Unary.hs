{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Unary
( 
  -- * size
  ropConsSize, relmapSize, relgenSize,
  -- * enclose
  ropConsEnclose, relmapEnclose, relgenEnclose,
  -- * rank
  ropConsRank, relmapRank, relgenRank, -- limit,
  -- * typename
  ropConsTypename, relmapTypename, relgenTypename,
  -- * range
  ropConsRange, relmapRange,
  -- * member
  -- $member
  ropConsMember, relmapMember, relgenMember,
  -- * RDF
  ropConsRdf
) where

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

relmapSize :: (C.CDec c) => C.RopUse c -> String -> C.Relmap c
relmapSize use n = C.relmapCalc use "size" gen where
    gen _ _ = relgenSize n

{-| Cardinality -}
relgenSize
    :: (C.CDec c)
    => String              -- ^ Name of new term
    -> B.Ab (C.Relgen c)   -- ^ Generator of output relation
relgenSize n = Right $ C.Relgen h2 (C.RelgenFull f) where
    h2 = B.headFrom [n]
    f b1 = [[C.putDecFromInt $ length b1]]



-- ----------------------  enclose

ropConsEnclose :: C.RopCons VContent
ropConsEnclose use =
  do n <- Builtin.getTerm use "-term"
     Right $ relmapEnclose use n

relmapEnclose :: (C.CRel c) => C.RopUse c -> String -> C.Relmap c
relmapEnclose use n = C.relmapCalc use "enclose" gen where
    gen _ = relgenEnclose n

{-| Enclose the current relation in a term. -}
relgenEnclose
    :: (C.CRel c)
    => String              -- ^ Termname of enclosed relation
    -> B.Relhead           -- ^ Header of input relation
    -> B.Ab (C.Relgen c)   -- ^ Generator of output relation
relgenEnclose n h1 = Right $ C.Relgen h2 (C.RelgenFull f) where
    h2 = B.Relhead [B.Nest n $ B.headTerms h1]
    f b1 = [[C.putRel $ B.Rel h1 b1]]



-- ----------------------  rank

ropConsRank :: C.RopCons VContent
ropConsRank use =
    do n  <- Builtin.getTerm  use "-add"
       ns <- Builtin.getTerms use "-order"
       Right $ relmapRank use n ns

relmapRank :: (C.CDec c, Ord c) => C.RopUse c -> String -> [String] -> C.Relmap c
relmapRank use n ns = C.relmapCalc use "rank" gen where
    gen _ = relgenRank n ns

relgenRank
    :: (Ord c, C.CDec c)
    => String              -- ^ Name of rank term
    -> [String]            -- ^ Termnames for order
    -> B.Relhead           -- ^ Header of input relation
    -> B.Ab (C.Relgen c)   -- ^ Generator for output relation
relgenRank n ns h1 = Right $ C.Relgen h2 $ C.RelgenFull g2 where
    h2    = n `B.headCons` h1
    g2 b1 = let b1' = sortByName ords (B.headNames h1) b1
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

relmapTypename :: (C.CContent c) => C.RopUse c -> String -> String -> C.Relmap c
relmapTypename use n p = C.relmapCalc use "typename" gen where
    gen _ = relgenTypename n p

{-| Get typename. -}
relgenTypename
  :: (C.CText c) =>
     String -> String -> B.Relhead -> B.Ab (C.Relgen c)
relgenTypename n p h1 = Right $ C.Relgen h2 (C.RelgenFull g2) where
    h2    = n `B.headCons` h1
    g2 b1 = map f b1
    pos   = h1 `B.posFlat` [p]
    f cs1 = let [c] = B.posPick pos cs1
            in C.putText (C.typename c) : cs1



-- ----------------------  range

ropConsRange :: C.RopCons VContent
ropConsRange use =
  do term <- Builtin.getTerm use "-term"
     low  <- Builtin.getInt  use "-from"
     high <- Builtin.getInt  use "-to"
     Right $ relmapRange use term low high

relmapRange :: (C.CDec c) => C.RopUse c -> String -> Int -> Int -> C.Relmap c
relmapRange use term low high = C.relmapCalc use "range" gen where
    gen _ = relgenRange term low high

relgenRange
  :: (C.CDec c) =>
     String -> Int -> Int -> B.Relhead -> B.Ab (C.Relgen c)
relgenRange term low high h1 = Right $ C.Relgen h2 (C.RelgenFull g2) where
    h2    = term `B.headCons` h1
    g2 b1 = concatMap g b1
    ys    = map C.putDecFromInt [low .. high]
    g xs  = map (: xs) ys



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

relmapMember :: C.RopUse VContent -> String -> String -> C.Relmap VContent
relmapMember use x xs = C.relmapCalc use "member" gen where
    gen _ = relgenMember x xs

relgenMember :: String -> String -> B.Relhead -> B.Ab (C.Relgen VContent)
relgenMember x xs h1 = r2 where
    r2 | xHere && xsHere     = relgenMemberCheck  xPos xsPos h1
       | not xHere && xsHere = relgenMemberExpand x    xsPos h1
       | otherwise           = Left $ B.AbortNoTerms [x, xs]
    ([xPos, xsPos], [xHere, xsHere])
        = h1 `B.posHere` [x, xs]

relgenMemberCheck :: B.TermPos -> B.TermPos -> B.Relhead -> B.Ab (C.Relgen VContent)
relgenMemberCheck xPos xsPos h1 = Right $ C.Relgen h1 (C.RelgenFull g2) where
    g2 b1  =  filter f b1
    f cs   =  let [xCont, xsCont] = B.posPick [xPos, xsPos] cs
              in xCont `isMember` xsCont

relgenMemberExpand :: String -> B.TermPos -> B.Relhead -> B.Ab (C.Relgen VContent)
relgenMemberExpand x xsPos h1 = Right $ C.Relgen h2 (C.RelgenFull g2) where
    h2     =  B.headCons x h1
    g2 b1  =  concatMap f b1
    f cs   =  let [xsCont] = B.posPick [xsPos] cs
              in case xsCont of
                   VSet  xs  -> map (: cs) xs
                   VList xs  -> map (: cs) xs
                   _         -> [xsCont : cs]



-- ----------------------  RDF

ropConsRdf :: C.RopCons VContent
ropConsRdf use =
    do sign  <- Builtin.getWord  use "-sign"
       [s,o] <- Builtin.getTerms use "-term"
       Right $ C.relmapAlias use $
             C.relmapSource use sign ["/s", "/o"] `Builtin.mappend`
             Mini.relmapRename use [(s,"/s"), (o,"/o")]

